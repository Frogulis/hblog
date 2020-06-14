module BlogConfig where

import Data.Aeson (decode, encode)

data RepoConnDetails = FileRepoDetails { filePath :: String }
                     | SqliteRepoDetails { dbName :: String }

data BlogConfig = BlogConfig { title        :: String
                             , maintainer   :: String
                             , description  :: String
                             , connDetails  :: RepoConnDetails
                             }
