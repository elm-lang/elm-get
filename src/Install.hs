module Install where

import Control.Monad.Error
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
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
    | Latest String
    | Exactly String V.Version


install :: Bool -> Args -> Manager.Manager ()
install autoYes args =
   do exists <- liftIO (doesFileExist Path.description)

      description <-
          case exists of
            True -> Desc.read Path.description
            False -> initialDescription

      let install' name version =
            do  newDescription <- addConstraint autoYes name version description
                upgrade autoYes newDescription
          getName str =
            do  isFolder <- liftIO (doesDirectoryExist str)
                case isFolder of
                    True -> do cPath <- liftIO (canonicalizePath str)
                               N.fromString'("local://" ++ cPath)
                    False -> N.fromString' str

      case args of
        Everything ->
            upgrade autoYes description

        Latest str ->
          do  name <- getName str
              version <- latestVersion name
              install' name version

        Exactly str version ->
          do  name <- getName str
              install' name version



-- INSTALL EVERYTHING

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


-- MODIFY DESCRIPTION

latestVersion :: N.Name -> Manager.Manager V.Version
latestVersion name = case name of
  N.Remote _ _ ->
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
  N.Local path ->
    do  exists <- liftIO $ doesFileExist (path </> Path.description)
        case exists of
          True -> do description <- Desc.read (path </> Path.description)
                     return $ Desc.version description
          False -> return $ Desc.version Desc.defaultDescription


addConstraint :: Bool -> N.Name -> V.Version -> Desc.Description -> Manager.Manager Desc.Description
addConstraint autoYes name version description =
  case List.lookup name (Desc.dependencies description) of
    Nothing ->
      addNewDependency autoYes name version description

    Just constraint
      | Constraint.isSatisfied constraint version ->
          return description

      | otherwise ->
          throwError $
            "This is a tricky update, you should modify " ++ Path.description ++ " yourself.\n"
            ++ "Package " ++ N.toString name ++ " is already listed as a dependency:\n\n    "
            ++ showDependency name constraint ++ "\n\n"
            ++ "You probably want one of the following constraints instead:\n\n    "
            ++ Constraint.toString (Constraint.expand constraint version) ++ "\n    "
            ++ Constraint.toString (Constraint.minimalRangeFrom version) ++ "\n"


addNewDependency :: Bool -> N.Name -> V.Version -> Desc.Description -> Manager.Manager Desc.Description
addNewDependency autoYes name version description =
  do  confirm <-
          case autoYes of
            True -> return True
            False ->
              do  answer <- liftIO confirmNewAddition
                  liftIO (putStrLn "")
                  return answer

      case confirm of
        False ->
          do  liftIO $ putStrLn noConfirmation
              return description
        True ->
          do  let newDescription = description { Desc.dependencies = newConstraints }
              liftIO $ Desc.write newDescription
              return newDescription
  where
    newConstraint =
        Constraint.minimalRangeFrom version

    newConstraints =
        (name, newConstraint) : Desc.dependencies description

    noConfirmation =
        "Cannot install the new package unless it appears in " ++ Path.description ++ ".\n" ++
        "If you do not like the constraint I suggested, change it manually and then run:\n" ++
        "\n    elm-package install\n\n" ++
        "This will install everything listed in " ++ Path.description ++ "."

    confirmNewAddition =
      do  putStrLn $
            "To install " ++ N.toString name ++ " I would like to add the following\n"
            ++ "dependency to " ++ Path.description ++ ":\n\n    "
            ++ showDependency name newConstraint
            ++ "\n"

          putStr $ "May I add that to " ++ Path.description ++ " for you? (y/n) "
          Cmd.yesOrNo


showDependency :: N.Name -> Constraint.Constraint -> String
showDependency name constraint =
    show (N.toString name) ++ ": " ++ show (Constraint.toString constraint)


initialDescription :: Manager.Manager Desc.Description
initialDescription =
  do  let core = N.Remote "elm-lang" "core"
      version <- latestVersion core
      let desc = Desc.defaultDescription {
          Desc.dependencies = [ (core, Constraint.minimalRangeFrom version) ]
      }
      liftIO (Desc.write desc)
      return desc
