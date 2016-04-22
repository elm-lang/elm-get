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


solve :: [(Package.Name, C.Constraint)] -> Manager.Manager S.Solution
solve constraints =
    do  store <- Store.initialStore
        maybeSolution <- evalStateT (exploreConstraints constraints) store
        case maybeSolution of
          Right solution -> return solution
          Left err ->
              throwError $
                 "Unable to find a set of packages that will work with your constraints: \n"
              ++ "\t" ++ show err


-- EXPLORE CONSTRAINTS

type Explorer a =
    StateT Store.Store Manager.Manager a


type Packages =
    Map.Map Package.Name [Package.Version]


data PackageError
  = NoCompilerVersion C.Constraint
  | PackageNotSatisfied
      Package.Name (Either C.Constraint [Package.Version])

instance Show PackageError where
  show (NoCompilerVersion c) = "Compiler version: `" ++ C.toString c ++ "` incompatible."
  show (PackageNotSatisfied name ecvs) = "Package `" ++ Package.toString name ++ "` not satisfiable: "
    ++ case ecvs of
         Left c -> C.toString c
         Right vs -> show $ map Package.versionToString vs


exploreConstraints :: [(Package.Name, C.Constraint)] -> Explorer (Either PackageError S.Solution)
exploreConstraints constraints =
  do  maybeInitialPackages <- addConstraints Map.empty constraints
      let initialPackages = either (const Map.empty) id maybeInitialPackages
      explorePackages Map.empty initialPackages


explorePackages :: S.Solution -> Packages -> Explorer (Either PackageError S.Solution)
explorePackages solution availablePackages =
    case Map.minViewWithKey availablePackages of
      Nothing ->
          return (Right solution)

      Just ((name, versions), remainingPackages) ->
          exploreVersionList name versions solution remainingPackages


exploreVersionList :: Package.Name -> [Package.Version] -> S.Solution -> Packages -> Explorer (Either PackageError S.Solution)
exploreVersionList name versions solution remainingPackages =
    go (reverse (List.sort versions))
  where
    go versions' =
        case versions' of
          [] -> return $ Left $ PackageNotSatisfied name (Right versions)
          version : rest ->
              do  maybeSolution <- exploreVersion name version solution remainingPackages
                  case maybeSolution of
                    Left _ -> go rest
                    answer -> return answer


exploreVersion :: Package.Name -> Package.Version -> S.Solution -> Packages -> Explorer (Either PackageError S.Solution)
exploreVersion name version solution remainingPackages =
  do  (elmVersion, constraints) <- Store.getConstraints name version
      if C.isSatisfied elmVersion Compiler.version
        then explore constraints
        else return $ Left (NoCompilerVersion elmVersion)

  where
    explore constraints =
      do  let (overlappingConstraints, newConstraints) =
                  List.partition (\(name, _) -> Map.member name solution) constraints

          case all (satisfiedBy solution) overlappingConstraints of
            False -> return $
                         let (name', constraint') = head $ dropWhile (satisfiedBy solution) overlappingConstraints
                         in  Left $ PackageNotSatisfied name' (Left constraint')
            True ->
              do  maybePackages <- addConstraints remainingPackages newConstraints
                  case maybePackages of
                    Right extendedPackages ->
                        explorePackages (Map.insert name version solution) extendedPackages
                    Left err -> return $ Left err


satisfiedBy :: S.Solution -> (Package.Name, C.Constraint) -> Bool
satisfiedBy solution (name, constraint) =
    case Map.lookup name solution of
      Nothing -> False
      Just version ->
          C.isSatisfied constraint version


addConstraints :: Packages -> [(Package.Name, C.Constraint)] -> Explorer (Either PackageError Packages)
addConstraints packages constraints =
    case constraints of
      [] -> return (Right packages)
      (name, constraint) : rest ->
          do  versions <- Store.getVersions name
              case filter (C.isSatisfied constraint) versions of
                [] -> return $ Left (PackageNotSatisfied name $ Left constraint)
                vs -> addConstraints (Map.insert name vs packages) rest
