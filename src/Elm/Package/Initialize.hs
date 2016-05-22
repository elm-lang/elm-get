{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Initialize (solution) where

import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import qualified Data.Map as Map
import System.Directory (doesFileExist)

import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as S
import qualified Install
import qualified Manager


solution :: (MonadError String m, MonadIO m) => Bool -> Bool -> m S.Solution
solution verbose autoYes =
    runInstall verbose autoYes Install.Everything


runInstall
    :: (MonadError String m, MonadIO m)
    => Bool
    -> Bool
    -> Install.Args
    -> m S.Solution
runInstall verbose autoYes args =
  do  either <- liftIO $ do
          env <- Manager.defaultEnvironment
          Manager.run env (Install.install verbose autoYes args)

      case either of
        Left err -> throwError err
        Right () ->
          do  exists <- liftIO (doesFileExist Path.solvedDependencies)
              if exists
                  then S.read Path.solvedDependencies
                  else return Map.empty
