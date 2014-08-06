{-# LANGUAGE OverloadedStrings #-}
module Get.Publish where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.Aeson (decode)
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.Directory
import System.Exit
import System.IO

import qualified Elm.Internal.Dependencies as D
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Assets as A
import qualified Elm.Internal.Version as V

import Get.Dependencies (defaultDeps)
import qualified Get.Registry as R
import qualified Utils.Http as Http
import qualified Utils.Commands as Cmd
import qualified Utils.Paths as Path
import qualified Utils.SemverCheck as Semver

publish :: ErrorT String IO ()
publish =
  do deps <- getDeps
     let name = D.name deps
         version = D.version deps
         exposedModules = D.exposed deps
     Cmd.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyElmVersion (D.elmVersion deps)
     verifyMetadata deps
     verifyExposedModulesExist exposedModules
     prev <- verifyVersion name version
     withCleanup $
       do generateDocs exposedModules
          case prev of
            Just prevVersion -> compareDocs name prevVersion
            Nothing -> return ()
          R.register name version Path.combinedJson
     Cmd.out "Success!"

exitAtFail :: ErrorT String IO a -> ErrorT String IO a
exitAtFail action =
  do either <- liftIO $ runErrorT $ action
     case either of
       Right deps -> return deps
       Left err ->
           liftIO $ do hPutStrLn stderr $ "\nError: " ++ err
                       exitFailure

getDeps :: ErrorT String IO D.Deps
getDeps = exitAtFail $ D.depsAt A.dependencyFile

withCleanup :: ErrorT String IO () -> ErrorT String IO ()
withCleanup action =
    do existed <- liftIO $ doesDirectoryExist "docs"
       either <- liftIO $ runErrorT action
       when (not existed) $ liftIO $ removeDirectoryRecursive "docs"
       case either of
         Left err -> throwError err
         Right () -> return ()

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
      _  -> throwError $ "Some of the fields in " ++ A.dependencyFile ++
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

verifyVersion :: N.Name -> V.Version -> ErrorT String IO (Maybe V.Version)
verifyVersion name version =
    do response <- R.versions name
       prevVersion <-
         case response of
           Nothing -> return Nothing
           Just versions ->
             do let prevVersion = maximum $ filter (<= version) versions
                checkSemanticVersioning prevVersion version
                return (Just prevVersion)

       checkTag version
       return prevVersion

    where
      checkSemanticVersioning (V.V prev _) (V.V curr _) =
        case Semver.immediateNext prev curr of
          Right _ -> return ()
          Left (pos, err) ->
            throwError $ case err of
              Semver.TooLongVersion ->
                concat ["You should use no more than three indices"
                       , "in your version number"]
              Semver.NotZero nz ->
                concat ["Expected zero at ", show pos, " got ", show nz]
              Semver.NotSuccessor v1 v2 ->
                concat ["Version at ", show pos, " changed from ", show v1
                       , " to ", show v2, ", which is incorrect - you must "
                       , "increase version number at most by one"]
              Semver.SameVersions ->
                unlines $
                [ "This version has already been released!"
                , "Increase patch number from latest version to release in current minor branch" ]

      checkTag version =
        do tags <- lines <$> Cmd.git [ "tag", "--list" ]
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
    do Cmd.run "elm" ("--make" : "--generate-docs" : elms)
       liftIO $
         do let path = Path.combinedJson
            BS.writeFile path "[\n"
            let addCommas = List.intersperse (BS.appendFile path ",\n")
            sequence_ $ addCommas $ map append jsons
            BS.appendFile path "\n]"

    where
      elms = map Path.moduleToElmFile modules
      jsons = map Path.moduleToJsonFile modules

      append :: FilePath -> IO ()
      append path =
        do json <- BS.readFile path
           BS.length json `seq` return ()
           BS.appendFile Path.combinedJson json

compareDocs :: N.Name -> V.Version -> ErrorT String IO ()
compareDocs name version =
  let url = concat [R.domain, "/catalog/", N.toFilePath name, "/"
                   , V.toString version, "/docs.json"]
  in
  do mv1 <- liftIO $ decode <$> BS.readFile Path.combinedJson
     v1 <-
       case mv1 of
         Just result -> return result
         Nothing -> throwError "Parse error while reading local docs.json"
     v2 <- Http.decodeFromUrl url

     case AT.parseEither Semver.buildDocsComparison (v1, v2) of
       Left err -> throwError err
       Right result -> liftIO $ print result
