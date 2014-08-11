{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Utils.ResolveDeps where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson (decode, FromJSON, ToJSON, encode)
import Data.List (foldl')
import Data.Map (Map)
import GHC.Generics (Generic)
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
import qualified Utils.Helpers as Helpers
import qualified Utils.Http as Http
import qualified Utils.Cache as Cache

-- | Try to read a JSON-encoded value from a file
decodeFromFile :: (MonadIO m, FromJSON a) => FilePath -> m (Maybe a)
decodeFromFile fullPath =
  do fileExists <- liftIO $ Dir.doesFileExist fullPath
     case fileExists of
       False -> return Nothing
       True ->
         do fileContents <- liftIO $ BS.readFile fullPath
            case decode fileContents of
              Nothing ->
                do liftIO $ putStrLn $ "Cache contents at " ++ fullPath ++ " are malformed"
                   return Nothing
              result@Just{} -> return result

-- | Try to write a value to a file in particular directory, encoding it to JSON
encodeToFile :: ToJSON a => a -> FilePath -> FilePath -> IO ()
encodeToFile value dir fileName =
  do Dir.createDirectoryIfMissing True dir
     BS.writeFile (dir </> fileName) (encode value)

-- | A wrapper around Utils.Cache.cacheComputation to simplify implementing
--   caching for downloaded packages metadata
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

-- | Library description as used in library.elm-lang.org/libraries.json
data LibraryInfo = LibraryInfo
    { name :: String
    , summary :: String
    , versions :: [V.Version]
    } deriving (Show, Generic)

instance FromJSON LibraryInfo
instance ToJSON LibraryInfo

type LibraryDB = Map String LibraryInfo

-- | Read information about libraries, probably from local cache
readLibraries :: ErrorT String IO LibraryDB
readLibraries =
  let dir = A.packagesDirectory </> "_elm_get_cache"
      fileName = "libraries.json"
      downloadAction = Http.decodeFromUrl $ Reg.domain ++ "/libraries.json"
  in fmap (Helpers.buildMap name) $ cacheWrapper downloadAction dir fileName

firstSuccess :: MonadError e m => e -> [m a] -> m a
firstSuccess err = foldl' catchIgnore (throwError err)
  where catchIgnore x y = catchError x (const y)

readDependencies :: String -> V.Version -> ErrorT String IO D.Deps
readDependencies name version =
  let url fname =
        concat [ Reg.domain , "/catalog/"
               , name
               , "/", show version
               , "/", fname
               ]
      urls = [url A.dependencyFile, url "elm_dependencies.json"]
      dir = A.packagesDirectory </> "_elm_get_cache" </> name
      fileName = show version ++ ".json"
      msg = "Error while downloading package " ++ show name ++ " information"
      downloadAction = firstSuccess msg $ map Http.decodeFromUrl urls
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

getDependencies :: N.Name -> V.Version -> SolverContext D.Deps
getDependencies name version =
  do libsMap <- gets ssLibrariesMap
     case M.lookup (name, version) libsMap of
       Just deps -> return deps
       Nothing ->
         do deps <- lift . lift $ readDependencies (N.toFilePath name) version
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
  do libraryDb <- readLibraries
     let unreader = runReaderT (solveConstraintsByDeps deps) libraryDb
         initialState = SolverState M.empty M.empty
     (solved, state) <- runStateT unreader initialState
     case solved of
       False -> throwError "Failed to satisfy all the constraints :-("
       True ->
         let result = M.delete (D.name deps) $ ssPinnedVersions state
         in return (M.toList result)
