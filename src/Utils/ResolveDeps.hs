{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Utils.ResolveDeps where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson
import Data.Functor ((<$>))
import Data.List (foldl')
import Data.Map (Map)
import GHC.Generics (Generic)
import Network.HTTP.Client (responseBody, httpLbs)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified System.Directory as Dir

import qualified Elm.Internal.Constraint as C
import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Assets as A
import qualified Elm.Internal.Version as V
import qualified Get.Registry as Reg
import qualified Utils.Http as Http
import qualified Utils.Cache as Cache

-- | Try to read a JSON-encoded value from a file
decodeFromFile :: (Functor m, MonadIO m, FromJSON a) => FilePath -> m (Maybe a)
decodeFromFile fullPath =
  do fileExists <- liftIO $ Dir.doesFileExist fullPath
     case fileExists of
       False -> return Nothing
       True -> decode <$> (liftIO $ BS.readFile fullPath)

-- | Try to write a value to a file in particular directory, encoding it to JSON
encodeToFile :: ToJSON a => a -> FilePath -> FilePath -> IO ()
encodeToFile value dir fileName =
  do Dir.createDirectoryIfMissing True dir
     BS.writeFile (dir </> fileName) (encode value)

{-| A wrapper around Utils.Cache.cacheComputation to simplify implementing
caching for downloaded packages metadata -}
cacheWrapper :: (FromJSON a, ToJSON a, MonadIO m) => m a -> FilePath -> FilePath -> m a
cacheWrapper uncached dir fileName =
  let readSomething =
        do let fullPath = dir </> fileName
           decodeFromFile fullPath
      writeSomething x = encodeToFile x dir fileName
  in Cache.cacheComputation
     uncached
     (liftIO $ readSomething)
     (\x -> liftIO $ writeSomething x)

-- | Try to read a JSON-encoded value from an URL. Throws an error in case of parse error
decodeFromUrl :: FromJSON a => String -> ErrorT String IO a
decodeFromUrl url =
  do result <- Http.send url $ \request manager ->
       fmap (decode . responseBody) $ httpLbs request manager
     case result of
       Just v -> return v
       Nothing -> throwError $ "Can't read value from " ++ url

data LibrarySource
  = CentralRepo
  | LocalDir FilePath
  deriving (Show, Generic)

-- | Library description as used in library.elm-lang.org/libraries.json
data LibraryInfo = LibraryInfo
    { name :: String
    , summary :: String
    , versions :: [V.Version]
    , source :: LibrarySource
    } deriving (Show, Generic)

replaceSource :: LibrarySource -> LibraryInfo -> LibraryInfo
replaceSource src info = info { source = src }

instance FromJSON LibraryInfo where
  parseJSON (Object o) =
    do name <- o .: "name"
       summary <- o .: "summary"
       versions <- o .: "versions"
       return $ LibraryInfo name summary versions CentralRepo
  parseJSON _ = fail "Trying to parse library info from non-object"

instance ToJSON LibraryInfo where
  toJSON info =
    object [ "name" .= name info
           , "summary" .= summary info
           , "versions" .= versions info ]

type LibraryDB = Map String LibraryInfo

buildMap :: Ord k => (v -> k) -> [v] -> Map k v
buildMap key values = foldl' (\map v -> M.insert (key v) v map) M.empty values

buildLocalDb :: [LibraryInfo] -> ErrorT String IO LibraryDB
buildLocalDb = foldM insertSafe M.empty
  where
    insertSafe m info =
      case M.lookup key m of
        Nothing -> return $ M.insert key info m
        Just _ -> throwError $ duplicateErr key
      where key = name info

    duplicateErr name =
      unlines
      [ "Library " ++ show name ++ " is stored more than once in local repo!"
      , "Resolve ambiguity and try again" ]

-- | For every minor version remove all patch versions but last
onlyLastPatches :: LibraryInfo -> LibraryInfo
onlyLastPatches info = info { versions = process $ versions info }
  where
    process ls =
      let insert v@(V.V parts _) = M.insertWith (++) (take 2 parts) [v]
          allByMinor = foldr insert M.empty ls
      in map maximum $ M.elems allByMinor

-- | Read information about libraries, probably from local cache
readLibraries :: D.Deps -> ErrorT String IO LibraryDB
readLibraries deps =
  do localLibs <- concat <$> mapM readLibrariesFromDir (D.sourceDirs deps)
     globalLibs <- readLibrariesFromCentral
     let localLibsDb = buildMap name localLibs
     -- Data.Map.union is left-biased, so local libs with same name will
     -- be preferred to list from global repository
     return $ M.union localLibsDb globalLibs

getLibraryInfo :: D.Deps -> LibrarySource -> LibraryInfo
getLibraryInfo deps src =
  LibraryInfo { name = N.toString (D.name deps)
              , summary = D.summary deps
              , versions = [D.version deps]
              , source = src }

readLibrariesFromDir :: FilePath -> ErrorT String IO [LibraryInfo]
readLibrariesFromDir dir =
  do maybeDeps <- decodeFromFile (dir </> A.dependencyFile)
     case maybeDeps of
       Nothing -> throwError (failedImport dir)
       Just deps ->
         do recList <- mapM readLibrariesFromDir (D.sourceDirs deps)
            return $ getLibraryInfo deps (LocalDir dir) : concat recList

  where
    failedImport dir =
      unlines
      [ "Failed to import a package from source directory " ++ dir ++ "!"
      , "Maybe " ++ A.dependencyFile ++ " is missing or malformed?" ]

readLibrariesFromCentral :: ErrorT String IO LibraryDB
readLibrariesFromCentral =
  let dir = A.packagesDirectory </> "_elm_get_cache"
      fileName = "libraries.json"
      downloadAction = decodeFromUrl $ Reg.domain ++ "/libraries.json"
  in
  do ls <- cacheWrapper downloadAction dir fileName
     return $ buildMap name $ map onlyLastPatches ls

readDependencies :: String -> V.Version -> ErrorT String IO D.Deps
readDependencies name version =
  let fullUrl = concat [ Reg.domain , "/catalog/"
                       , name
                       , "/", show version
                       , "/", A.dependencyFile
                       ]
      dir = A.packagesDirectory </> "_elm_get_cache" </> name
      fileName = show version ++ ".json"
      downloadAction = decodeFromUrl fullUrl
  in cacheWrapper downloadAction dir fileName

data SolverState = SolverState
    { ssLibrariesMap :: Map (N.Name, V.Version) D.Deps
    , ssPinnedVersions :: Map N.Name V.Version
    }

type SolverContext =
  ReaderT LibraryDB    -- information about libraries, stays constant
  (StateT SolverState  -- solver current state, also RAM-cached dependencies info
   (ErrorT String IO)) -- underlying effects for IO and errors

tryAny :: MonadState s m => (s -> s) -> [m Bool] -> m Bool
tryAny restore solutions =
  foldM processOne False solutions
    where processOne False action =
            do modify restore
               action
          processOne True _ = return True

tryAll :: MonadState s m => [m Bool] -> m Bool
tryAll solutions =
  foldM processOne True solutions
    where processOne True action = action
          processOne False _ = return False

restorePinned :: Map N.Name V.Version -> (SolverState -> SolverState)
restorePinned pinned s = s { ssPinnedVersions = pinned }

-- | Change all occurrences of first element to second in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace c1 c2 = map (\x -> if x == c1 then c2 else x)

resolvableName :: N.Name -> String
resolvableName = replace '/' '-' . show

getDependencies :: N.Name -> V.Version -> SolverContext D.Deps
getDependencies name version =
  do libsMap <- gets ssLibrariesMap
     case M.lookup (name, version) libsMap of
       Just deps -> return deps
       Nothing ->
         do deps <- lift . lift $ readDependencies (resolvableName name) version
            modify (\s -> s { ssLibrariesMap = M.insert (name, version) deps libsMap })
            return deps

tryFromJust :: MonadError String m => String -> Maybe a -> m a
tryFromJust msg value = case value of
  Just x -> return x
  Nothing -> throwError msg

solveConstraintsByName :: N.Name -> C.Constraint -> SolverContext Bool
solveConstraintsByName name constr =
  do pinnedVersions <- gets ssPinnedVersions
     case M.lookup name pinnedVersions of
       Just currV -> return (C.satisfyConstraint constr currV)
       Nothing ->
         do maybeVersions <- asks (fmap versions . M.lookup (show name))
            let msg = "Haven't found versions of " ++ show name ++ ", halting"
            versions <- tryFromJust msg maybeVersions
            let restore = restorePinned pinnedVersions
                tryOne version =
                  do deps <- getDependencies name version
                     solveConstraintsByDeps deps
            tryAny restore $ map tryOne $ filter (C.satisfyConstraint constr) versions

solveConstraintsByDeps :: D.Deps -> SolverContext Bool
solveConstraintsByDeps deps =
  do pinned <- fmap (M.insert (D.name deps) (D.version deps)) $ gets ssPinnedVersions
     let tryOne (name, constr) = solveConstraintsByName name constr
     modify (restorePinned pinned)
     tryAll $ map tryOne $ D.dependencies deps

solveConstraints :: D.Deps -> ErrorT String IO [(N.Name, V.Version)]
solveConstraints deps =
  do libraryDb <- readLibraries deps
     let unreader = runReaderT (solveConstraintsByDeps deps) libraryDb
         initialState = SolverState M.empty M.empty
     (solved, state) <- runStateT unreader initialState
     case solved of
       False -> throwError "Failed to satisfy all the constraints :-("
       True ->
         let result = M.delete (D.name deps) $ ssPinnedVersions state
         in return (M.toList result)
