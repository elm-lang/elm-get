module Publish where

import Control.Monad.Error.Class (throwError)
import qualified Data.Maybe as Maybe

import qualified Bump
import qualified Catalog
import qualified CommandLine.Helpers as Cmd
import qualified Docs
import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Package
import qualified Elm.Package.Paths as P
import qualified GitHub
import qualified Manager


publish :: Manager.Manager ()
publish =
  do  description <- Desc.read P.description

      name <-
          case Desc.name description of
            Nothing -> throwError "Cannot publish with no package name!  You may need to add a repository to your elm-package.json, something like https://github.com/USER/PROJECT.git"
            Just n -> return n
      let version = Desc.version description

      Cmd.out $ unwords [ "Verifying", Package.toString name, Package.versionToString version, "..." ]
      verifyMetadata description

      docs <- Docs.generate

      validity <- verifyVersion docs description
      newVersion <-
          case validity of
            Bump.Valid -> return version
            Bump.Invalid -> throwError "Cannot publish with an invalid version!"
            Bump.Changed v -> return v

      verifyTag name newVersion
      Catalog.register name newVersion
      Cmd.out "Success!"



verifyMetadata :: Desc.Description -> Manager.Manager ()
verifyMetadata deps =
    case problems of
      [] -> return ()
      _  ->
          throwError $
          "Some of the fields in " ++ P.description ++
          " have not been filled in yet:\n\n" ++ unlines problems ++
          "\nFill these in and try to publish again!"
    where
      problems = Maybe.catMaybes
          [ verify Desc.repo "  repository - must refer to a valid repo on GitHub"
          , verify Desc.summary "  summary - a quick summary of your project, 80 characters or less"
          , verify Desc.exposed "  exposed-modules - list modules your project exposes to users"
          ]

      verify getField msg =
          if getField deps == getField Desc.defaultDescription
            then Just msg
            else Nothing


verifyVersion
    :: [Docs.Documentation]
    -> Desc.Description
    -> Manager.Manager Bump.Validity
verifyVersion docs description =
  let name = Maybe.fromMaybe (Package.Name "USER" "PROJECT") (Desc.name description)
      version = Desc.version description
  in
  do  maybeVersions <- Catalog.versions name
      case maybeVersions of
        Just publishedVersions ->
            Bump.validateVersion docs name version publishedVersions

        Nothing ->
            Bump.validateInitialVersion description


verifyTag :: Package.Name -> Package.Version -> Manager.Manager ()
verifyTag name version =
    do  publicVersions <- GitHub.getVersionTags name
        if version `elem` publicVersions
            then return ()
            else throwError (tagMessage version)


tagMessage :: Package.Version -> String
tagMessage version =
    let v = Package.versionToString version in
    unlines
    [ "Libraries must be tagged in git, but tag " ++ v ++ " was not found."
    , "These tags make it possible to find this specific version on github."
    , "To tag the most recent commit and push it to github, run this:"
    , ""
    , "    git tag -a " ++ v ++ " -m \"release version " ++ v ++ "\""
    , "    git push origin " ++ v
    , ""
    ]
