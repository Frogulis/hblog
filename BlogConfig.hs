module BlogConfig where

import Repository (RepoConnDetails)

data BlogConfig = BlogConfig { title        :: String
                             , maintainer   :: String
                             , description  :: String
                             , connDetails  :: RepoConnDetails
                             }
