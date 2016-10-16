{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Initialize (solution) where

import Control.Monad.Except (ExceptT, MonadIO, liftIO, throwError)
import qualified Data.Map as Map
import System.Directory (doesFileExist)

import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as S
import qualified Install
import qualified Manager
import qualified Reporting.Error as Error



solution :: (MonadIO m) => String -> Bool -> ExceptT String m S.Solution
solution host autoYes =
  do  result <- liftIO $ Manager.run $ installEverythingAndGetSolution host autoYes
      case result of
        Right solution ->
          return solution

        Left err ->
          throwError $ Error.toString err


installEverythingAndGetSolution :: String -> Bool -> Manager.Manager S.Solution
installEverythingAndGetSolution host autoYes =
  do  () <- Install.install autoYes host Install.Everything
      exists <- liftIO (doesFileExist Path.solvedDependencies)
      if exists
        then S.read Error.CorruptSolution Path.solvedDependencies
        else return Map.empty
