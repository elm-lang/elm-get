{-# LANGUAGE OverloadedStrings #-}
module Get.Publish where

import Control.Applicative ((<$>))
import Control.Monad.Error
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.Directory
import System.Exit
import System.IO

import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Libraries as L
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Paths as EPath
import qualified Elm.Internal.Version as V

import Get.Dependencies (defaultDeps)
import qualified Get.Registry as R
import qualified Utils.Commands as Cmd
import qualified Utils.Paths as Path

data VersionError
  = NotSuccessor Int Int
  | NotZero Int
  | SameVersions
  deriving (Show)

type IndexPos = Int

-- | Check two version numbers to see whether one of them follows another
--   If that's the case, return an index in version number when increment
--   takes place. If that's not the case, return an error with position
--   where that error happened
immediateNext :: [Int] -> [Int] -> Either (IndexPos, VersionError) IndexPos
immediateNext prev next = check 0 $ zip (prev ++ repeat 0) next
  where check i ls =
          case ls of
            [] -> Left (i, SameVersions)
            (x, y) : rest
              | x == y -> check (i + 1) rest
              | x + 1 == y -> checkZeros i (i + 1) rest
              | otherwise -> Left (i, NotSuccessor x y)

        checkZeros result pos ls =
          case ls of
            [] -> Right result
            (_, 0) : rest -> checkZeros result (pos + 1) rest
            (_, y) : _ -> Left (pos, NotZero y)

showIndexPos :: IndexPos -> String
showIndexPos x =
  case x of
    0 -> "major position"
    1 -> "minor position"
    2 -> "patch position"
    _ -> "position " ++ show x

publish :: ErrorT String IO ()
publish =
  do deps <- getDeps
     versions <- getVersions
     let name = D.name deps
         version = D.version deps
         exposedModules = D.exposed deps
     Cmd.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyNoDependencies versions
     verifyElmVersion (D.elmVersion deps)
     verifyMetadata deps
     verifyExposedModulesExist exposedModules
     verifyVersion name version
     withCleanup $ do
       generateDocs exposedModules
       R.register name version Path.combinedJson
     Cmd.out "Success!"

exitAtFail :: ErrorT String IO a -> ErrorT String IO a
exitAtFail action =
  do either <- liftIO $ runErrorT $ action
     case either of
       Right deps -> return deps
       Left err ->
           liftIO $ do
             hPutStrLn stderr $ "\nError: " ++ err
             exitFailure

getDeps :: ErrorT String IO D.Deps
getDeps = exitAtFail $ D.depsAt EPath.dependencyFile

getVersions :: ErrorT String IO [(N.Name, V.Version)]
getVersions = exitAtFail $ L.getVersions EPath.librariesFile

withCleanup :: ErrorT String IO () -> ErrorT String IO ()
withCleanup action =
    do existed <- liftIO $ doesDirectoryExist "docs"
       either <- liftIO $ runErrorT action
       when (not existed) $ liftIO $ removeDirectoryRecursive "docs"
       case either of
         Left err -> throwError err
         Right () -> return ()

verifyNoDependencies :: [(N.Name,V.Version)] -> ErrorT String IO ()
verifyNoDependencies [] = return ()
verifyNoDependencies _ =
    throwError
        "elm-get is not able to publish projects with dependencies yet. This is a\n\
        \very high proirity, we are working on it! For now, announce your library on the\n\
        \mailing list: <https://groups.google.com/forum/#!forum/elm-discuss>"

verifyElmVersion :: V.Version -> ErrorT String IO ()
verifyElmVersion elmVersion@(V.V ns _)
    | ns == ns' = return ()
    | otherwise =
        throwError $ "elm_dependencies.json says this project depends on version " ++
                     show elmVersion ++ " of the compiler but the compiler you " ++
                     "have installed is version " ++ show V.elmVersion
    where
      V.V ns' _ = V.elmVersion

verifyExposedModulesExist :: [String] -> ErrorT String IO ()
verifyExposedModulesExist modules =
      mapM_ verifyExists modules
    where
      verifyExists modul =
          let path = Path.moduleToElmFile modul in
          do exists <- liftIO $ doesFileExist path
             when (not exists) $ throwError $
                 "Cannot find module " ++ modul ++ " at " ++ path

verifyMetadata :: D.Deps -> ErrorT String IO ()
verifyMetadata deps =
    case problems of
      [] -> return ()
      _  -> throwError $ "Some of the fields in " ++ EPath.dependencyFile ++
                         " have not been filled in yet:\n\n" ++ unlines problems ++
                         "\nFill these in and try to publish again!"
    where
      problems = Maybe.catMaybes
          [ verify D.repo        "  repository - must refer to a valid repo on GitHub"
          , verify D.summary     "  summary - a quick summary of your project, 80 characters or less"
          , verify D.description "  description - extended description, how to get started, any useful references"
          , verify D.exposed     "  exposed-modules - list modules your project exposes to users"
          ]

      verify what msg =
          if what deps == what defaultDeps
            then Just msg
            else Nothing

verifyVersion :: N.Name -> V.Version -> ErrorT String IO ()
verifyVersion name version =
    do response <- R.versions name
       case response of
         Nothing -> return ()
         Just versions ->
           do let prevVersion = maximum $ filter (<= version) versions
              checkSemanticVersioning prevVersion version

       checkTag version

    where
      checkSemanticVersioning (V.V prev _) (V.V curr _) =
        case immediateNext prev curr of
          Right _ -> return ()
          Left (pos, err) ->
            throwError $ case err of
              NotZero nz ->
                concat ["Expected zero at ", showIndexPos pos, " got ", show nz]
              NotSuccessor v1 v2 ->
                concat ["Version at ", showIndexPos pos, " changed from ", show v1
                       , " to ", show v2, ", which is incorrect - you must "
                       , "increase version number at most by one"]
              SameVersions ->
                unlines $
                [ "This version has already been released!"
                , "Increase patch number from latest version to release in current minor branch" ]

      checkTag version = do
        tags <- lines <$> Cmd.git [ "tag", "--list" ]
        let v = show version
        when (show version `notElem` tags) $
             throwError (unlines (tagMessage v))

      tagMessage v =
          [ "Libraries must be tagged in git, but tag " ++ v ++ " was not found."
          , "These tags make it possible to find this specific version on github."
          , "To tag the most recent commit and push it to github, run this:"
          , ""
          , "    git tag -a " ++ v ++ " -m \"release version " ++ v ++ "\""
          , "    git push origin " ++ v
          , ""
          ]

generateDocs :: [String] -> ErrorT String IO ()
generateDocs modules =
    do forM elms $ \path -> Cmd.run "elm-doc" [path]
       liftIO $ do
         let path = Path.combinedJson
         BS.writeFile path "[\n"
         let addCommas = List.intersperse (BS.appendFile path ",\n")
         sequence_ $ addCommas $ map append jsons
         BS.appendFile path "\n]"

    where
      elms = map Path.moduleToElmFile modules
      jsons = map Path.moduleToJsonFile modules

      append :: FilePath -> IO ()
      append path = do
        json <- BS.readFile path
        BS.length json `seq` return ()
        BS.appendFile Path.combinedJson json
