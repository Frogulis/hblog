{-# LANGUAGE OverloadedStrings #-}
module BlogConfig (RepoConnDetails(..), BlogConfig(..), loadConfig) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy as BL (readFile, ByteString)
import Data.Maybe (fromMaybe)

data RepoConnDetails = FileRepoDetails { filePath :: String }
                     | SqliteRepoDetails { dbName :: String } deriving (Show)

data BlogConfig = BlogConfig { title    :: String
                             , maintainer   :: String
                             , description  :: String
                             , connDetails  :: RepoConnDetails
                             } deriving (Show)

data RepoConnPOD = RepoConnPOD { repoType    :: String
                               , details     :: String
                               }

loadConfig :: String -> IO BlogConfig
loadConfig configFilename =
    either (\msg -> error $ msg) id .
    (eitherDecode :: ByteString -> Either String BlogConfig) <$>
    BL.readFile configFilename

instance FromJSON BlogConfig where
    parseJSON (Object o)    = BlogConfig
        <$> o .: "blogTitle"
        <*> o .: "maintainer"
        <*> o .: "description"
        <*> o .: "connDetails"
    parseJSON _             = error "Expected object in JSON file"

instance FromJSON RepoConnDetails where
    parseJSON (Object o)    = RepoConnPOD <$> o .: "repoType" <*> o .: "details" >>= convert
    parseJSON _             = error "Expected object for connDetails"

convert :: RepoConnPOD -> Parser RepoConnDetails
convert (RepoConnPOD "FILE" filename)   = return $ FileRepoDetails filename
convert (RepoConnPOD "SQLITE" name)     = return $ SqliteRepoDetails name
convert _                               = error "Invalid connection details"
