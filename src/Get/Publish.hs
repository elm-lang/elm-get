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
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Paths as EPath
import qualified Elm.Internal.Version as V

import Get.Dependencies (defaultDeps)
import qualified Get.Registry as R
import qualified Utils.Commands as Cmd
import qualified Utils.Paths as Path

publish :: ErrorT String IO ()
publish =
  do deps <- getDeps
     let name = D.name deps
         version = D.version deps
         exposedModules = D.exposed deps
     Cmd.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyNoDependencies (D.dependencies deps)
     verifyElmVersion (D.elmVersion [])
     verifyMetadata []
     verifyExposedModulesExist exposedModules
     verifyVersion name version
     withCleanup $ do
       generateDocs exposedModules
       R.register name version Path.combinedJson
     Cmd.out "Success!"

getDeps :: ErrorT String IO D.Deps
getDeps =
  do either <- liftIO $ runErrorT $ D.depsAt EPath.dependencyFile
     case either of
       Right deps -> return deps
       Left err ->
           liftIO $ do
             hPutStrLn stderr $ "\nError: " ++ err
             exitFailure

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
verifyNoDependencies _ = do
    Cmd.out $ "Warning: Dependencies are not supported, and will be ignored"
    return ()

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
             do let maxVersion = maximum (version:versions)
                when (version < maxVersion) $ throwError $ unlines
                     [ "a later version has already been released."
                     , "Use a version number higher than " ++ show maxVersion ]
                checkSemanticVersioning maxVersion

       checkTag version

    where
      checkSemanticVersioning _ = return ()

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
