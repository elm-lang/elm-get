module Install.Solver where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, evalStateT)
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Elm.Compiler as Compiler
import qualified Elm.Package.Constraint as C
import qualified Elm.Package as Package
import qualified Elm.Package.Solution as S
import qualified Manager
import qualified Store
import Logger


solve :: [(Package.Name, C.Constraint)] -> Manager.Manager S.Solution
solve constraints =
    do  store <- Store.initialStore
        maybeSolution <- evalStateT (exploreConstraints constraints) store
        case maybeSolution of
          Just solution -> return solution
          Nothing ->
              throwError $
              "Unable to find a set of packages that will work with your constraints."


-- EXPLORE CONSTRAINTS

type Explorer a =
    StateT Store.Store Manager.Manager a


type Packages =
    Map.Map Package.Name [Package.Version]


exploreConstraints :: [(Package.Name, C.Constraint)] -> Explorer (Maybe S.Solution)
exploreConstraints constraints =
  do  maybeInitialPackages <- addConstraints Map.empty constraints
      let initialPackages = maybe Map.empty id maybeInitialPackages
      explorePackages Map.empty initialPackages


explorePackages :: S.Solution -> Packages -> Explorer (Maybe S.Solution)
explorePackages solution availablePackages =
    case Map.minViewWithKey availablePackages of
      Nothing ->
          return (Just solution)

      Just ((name, versions), remainingPackages) ->
          exploreVersionList name versions solution remainingPackages


exploreVersionList :: Package.Name -> [Package.Version] -> S.Solution -> Packages -> Explorer (Maybe S.Solution)
exploreVersionList name versions solution remainingPackages =
    go (reverse (List.sort versions))
  where
    go versions =
        case versions of
          [] -> return Nothing
          version : rest ->
              do  maybeSolution <- exploreVersion name version solution remainingPackages
                  case maybeSolution of
                    Nothing -> go rest
                    answer -> return answer


exploreVersion :: Package.Name -> Package.Version -> S.Solution -> Packages -> Explorer (Maybe S.Solution)
exploreVersion name version solution remainingPackages =
  do  (elmVersion, constraints) <- Store.getConstraints name version
      if C.isSatisfied elmVersion Compiler.version
        then explore constraints
        else
          do  infoM $ (displayNameAndVersion name version) ++ " requires Elm version " ++ (C.toString elmVersion) ++ " but we are on " ++ (Package.versionToString Compiler.version)
              return Nothing

  where
    explore constraints =
      do  let (overlappingConstraints, newConstraints) =
                  List.partition (\(name, _) -> Map.member name solution) constraints

          case all (satisfiedBy solution) overlappingConstraints of
            False -> return Nothing
            True ->
              do  case newConstraints of
                    [] -> return Nothing
                    someConstraints ->
                        do  infoM $ (displayNameAndVersion name version) ++ " depends on " ++ (displayConstraints someConstraints)
                            return Nothing
                  maybePackages <- addConstraints remainingPackages newConstraints
                  case maybePackages of
                    Nothing -> return Nothing
                    Just extendedPackages ->
                        explorePackages (Map.insert name version solution) extendedPackages


satisfiedBy :: S.Solution -> (Package.Name, C.Constraint) -> Bool
satisfiedBy solution (name, constraint) =
    case Map.lookup name solution of
      Nothing -> False
      Just version ->
          C.isSatisfied constraint version


addConstraints :: Packages -> [(Package.Name, C.Constraint)] -> Explorer (Maybe Packages)
addConstraints packages constraints =
    case constraints of
      [] -> return (Just packages)
      (name, constraint) : rest ->
          do  versions <- Store.getVersions name
              case filter (C.isSatisfied constraint) versions of
                [] ->
                  do  infoM $ (Package.toString name) ++ " (" ++ (displayVersions versions) ++ ") has no package version that satisfies " ++ (C.toString constraint)
                      return Nothing
                vs ->
                  do  infoM $ (Package.toString name) ++ " = " ++ (displayVersions vs) ++ (displayNewerVersions (head vs) versions)
                      addConstraints (Map.insert name vs packages) rest


-- LOGGING


displayNamedConstraint :: (Package.Name, C.Constraint) -> String
displayNamedConstraint (name, constraint) =
  (Package.toString name) ++ ": " ++ (C.toString constraint)


displayNameAndVersion :: Package.Name -> Package.Version -> String
displayNameAndVersion name version =
  (Package.toString name) ++ " = " ++ (Package.versionToString version)


displayConstraints :: [(Package.Name, C.Constraint)] -> String
displayConstraints constraints =
  List.intercalate ", " (map displayNamedConstraint constraints)


displayVersions :: [Package.Version] -> String
displayVersions versions =
  List.intercalate ", " (map Package.versionToString versions)


displayNewerVersions :: Package.Version -> [Package.Version] -> String
displayNewerVersions current versions =
  do  case newerVersions of
        [] ->
          ""

        versions ->
          " < " ++ (displayVersions versions)

  where
    newerVersions =
      filter (\v -> v > current) versions
