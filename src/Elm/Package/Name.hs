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
    Name
        { user :: String
        , project :: String
        }
    | Local
        { absolutePath :: FilePath }
    deriving (Eq, Ord)

dummyName :: Name
dummyName =
    Name "USER" "PROJECT"


toString :: Name -> String
toString name = case name of
    Name user project -> user ++ "/" ++ project
    Local path -> "file://" ++ path


toUrl :: Name -> String
toUrl name = case name of
    Name user project -> user ++ "/" ++ project
    Local path -> "file://" ++ path


toFilePath :: Name -> FilePath
toFilePath name = case name of
    Name user project -> user </> project
    Local path -> path


fromString :: String -> Maybe Name
fromString string =
    case break (=='/') string of
      ( "file:", '/':'/': path@(_:_)) -> Just (Local path)
      ( user@(_:_), '/' : project@(_:_) )
          | all (/='/') project -> Just (Name user project)
      _ -> Nothing


fromString' :: (MonadError String m) => String -> m Name
fromString' string =
    Maybe.maybe (throwError $ errorMsg string) return (fromString string)


instance Binary Name where
    get = do t <- get :: Get Word8
             case t of
                0 -> Name <$> get <*> get
                1 -> Local <$> get
    put (Name user project) =
        do  put (0 :: Word8)
            put user
            put project
    put (Local path) =
        do  put (1 :: Word8)
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
