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
import qualified Install
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
            False -> Install.initialDescription

      case args of
        Everything ->
           do  Install.upgrade autoYes description