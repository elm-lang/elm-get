module Manager
  ( Manager
  , run
  , Environment(..)
  )
  where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Error (catchError, throwError)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Elm.Compiler as Elm
import qualified Elm.Package as Pkg
import qualified Network
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path

import qualified Reporting.Error as Error


type Manager =
  ExceptT Error.Error (ReaderT Environment IO)


run :: String -> Manager a -> IO (Either Error.Error a)
run path manager =
  Network.withSocketsDo $
    do  cacheDirectory <- getCacheDirectory

        let fullDescriptionPath = path </> Path.description

        exists <- liftIO (Dir.doesFileExist fullDescriptionPath)

        catalogUrl <-
          if exists then
            do desc <- Desc.maybeRead fullDescriptionPath
               return (getCatalogUrl desc)
          else
              return Path.defaultCatalogUrl

        httpManager <- Http.newManager Http.tlsManagerSettings

        let env = Environment catalogUrl cacheDirectory httpManager
        runReaderT (runExceptT manager) env


data Environment =
  Environment
    { catalog :: String
    , cacheDirectory :: FilePath
    , httpManager :: Http.Manager
    }


getCacheDirectory :: IO FilePath
getCacheDirectory =
  do  root <- Dir.getAppUserDataDirectory "elm"
      let dir = root </> Pkg.versionToString Elm.version </> "package"
      Dir.createDirectoryIfMissing True dir
      return dir


getCatalogUrl :: Either a Desc.Description -> String
getCatalogUrl perhapsDesc =
  case perhapsDesc of
    Left _ ->
      Path.defaultCatalogUrl
    Right desc ->
      Desc.catalogUrl desc
