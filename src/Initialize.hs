module Initialize where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (liftIO)
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))

import qualified CommandLine.Helpers as Cmd
import qualified Elm.Package.Constraint as Constraint
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution
import qualified Elm.Package.Version as V
import qualified Install.Fetch as Fetch
import qualified Install.Plan as Plan
import qualified Install.Solver as Solver
import qualified Manager
import qualified Store


data Args
    = Everything
    | Latest N.Name
    | Exactly N.Name V.Version


initialize :: Bool -> Args -> Manager.Manager ()
initialize autoYes args =
  do  exists <- liftIO (doesFileExist Path.description)

      description <-
          case exists of
            True -> Desc.read Path.description
            False -> initialDescription

      case args of
        Everything ->
           do  upgrade autoYes description


upgrade :: Bool -> Desc.Description -> Manager.Manager ()
upgrade autoYes description =
  do  newSolution <- Solver.solve (Desc.dependencies description)

      exists <- liftIO (doesFileExist Path.solvedDependencies)
      oldSolution <-
          if exists
              then Solution.read Path.solvedDependencies
              else return Map.empty

      let plan = Plan.create oldSolution newSolution

      approve <- liftIO (getApproval autoYes plan)

      if approve
          then runPlan newSolution plan
          else liftIO $ putStrLn "Okay, I did not change anything!"


getApproval :: Bool -> Plan.Plan -> IO Bool
getApproval autoYes plan =
  case autoYes || Plan.isEmpty plan of
    True ->
      return True

    False ->
      do  putStrLn "Some new packages are needed. Here is the upgrade plan."
          putStrLn (Plan.display plan)
          putStr "Do you approve of this plan? (y/n) "
          Cmd.yesOrNo

initialDescription :: Manager.Manager Desc.Description
initialDescription =
  do  let core = N.Name "elm-lang" "core"
      version <- latestVersion core
      let desc = Desc.defaultDescription {
          Desc.dependencies = [ (core, Constraint.untilNextMajor version) ]
      }
      liftIO (Desc.write desc)
      return desc


runPlan :: Solution.Solution -> Plan.Plan -> Manager.Manager ()
runPlan solution plan =
  do  let installs =
            Map.toList (Plan.installs plan)
            ++ Map.toList (Map.map snd (Plan.upgrades plan))

      let removals =
            Map.toList (Plan.removals plan)
            ++ Map.toList (Map.map fst (Plan.upgrades plan))

      -- fetch new dependencies
      Cmd.inDir Path.packagesDirectory $
          forM_ installs $ \(name, version) ->
              do  liftIO (putStrLn ("Downloading " ++ N.toString name))
                  Fetch.package name version

      -- try to build new dependencies
      liftIO (Solution.write Path.solvedDependencies solution)

      -- remove dependencies that are not needed
      Cmd.inDir Path.packagesDirectory $
          forM_ removals $ \(name, version) ->
              liftIO $ removeDirectoryRecursive (N.toFilePath name </> V.toString version)

      liftIO $ putStrLn "Packages configured successfully!"

latestVersion :: N.Name -> Manager.Manager V.Version
latestVersion name =
  do  versionCache <- Store.readVersionCache
      case Map.lookup name versionCache of
        Just versions ->
            return $ maximum versions

        Nothing ->
            throwError $
            unlines
            [ "No versions of package '" ++ N.toString name ++ "' were found!"
            , "Is it spelled correctly?"
            ]
