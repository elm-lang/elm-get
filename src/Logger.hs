module Logger where

import Control.Monad.Trans (MonadIO(..), liftIO)
import System.Log.Logger (updateGlobalLogger,
                          Priority(..),
                          rootLoggerName,
                          setLevel)
import qualified System.Log.Logger as L


configure :: Bool -> IO ()
configure verbose =
  do liftIO $ updateGlobalLogger rootLoggerName (setLevel logLevel)
  where logLevel = if verbose
                   then DEBUG
                   else WARNING


infoM :: MonadIO m => String -> m ()
infoM message =
  liftIO $ L.infoM rootLoggerName message
