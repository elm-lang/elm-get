{-# LANGUAGE FlexibleContexts #-}
module Elm.Package.Name where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error
import Data.Aeson
import Data.Binary
import qualified Data.Text as T
import qualified Data.Maybe as Maybe
import System.FilePath ((</>))


data Name =
    Remote
        { user :: String
        , project :: String
        }
    | Local
        { absolutePath :: FilePath }
    deriving (Eq, Ord)

dummyName :: Name
dummyName =
    Remote "USER" "PROJECT"


toString :: Name -> String
toString name = case name of
    Remote user project -> user ++ "/" ++ project
    Local path -> "local://" ++ path


toUrl :: Name -> String
toUrl name = case name of
    Remote user project -> user ++ "/" ++ project
    Local path -> "local://" ++ path


toFilePath :: Name -> FilePath
toFilePath name = case name of
    Remote user project -> user </> project
    Local path -> path


fromString :: String -> Maybe Name
fromString string =
    case break (=='/') string of
      ( "local:", '/':'/': path@(_:_)) -> Just (Local path)
      ( user@(_:_), '/' : project@(_:_) )
          | all (/='/') project -> Just (Remote user project)
      _ -> Nothing


fromString' :: (MonadError String m) => String -> m Name
fromString' string =
    Maybe.maybe (throwError $ errorMsg string) return (fromString string)


instance Binary Name where
    get = do t <- get :: Get String
             case t of
                ("local://") -> Local <$> get
                user        -> Remote user <$> get
    put (Remote user project) =
        do  put user
            put project
    put (Local path) =
        do  put ("local://" :: String)
            put path


instance FromJSON Name where
    parseJSON (String text) =
        let string = T.unpack text in
        Maybe.maybe (fail $ errorMsg string) return (fromString string)

    parseJSON _ = fail "Project name must be a string."


instance ToJSON Name where
    toJSON name =
        toJSON (toString name)


errorMsg :: String -> String
errorMsg string =
    unlines
    [ "Dependency file has an invalid name: " ++ string
    , "Must have format USER/PROJECT and match a public github project."
    ]
