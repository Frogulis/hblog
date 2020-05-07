{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Data.Text (Text)

import Happstack.Lite
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import BlogConfig
import Pages.Post
import Pages.Default (aboutPage, aboutHblogPage, homePage)
import Repository

serverConfig = defaultServerConfig { port = 8000 }

blogConfig = BlogConfig
    "Sample blog"
    "Jamie Hoffmann"
    "A sample blog about something blah blah.\nWhatever and ever!"


main :: IO ()
main = do
    putStrLn $ "Running at http://localhost:" ++ (show $ port serverConfig)
    serve (Just serverConfig) (myApp blogConfig)

-- load in config before this point so that it can be hot-reloaded while
-- separate from rest of application
myApp :: BlogConfig -> ServerPart Response
myApp config = do
    msum
        [ dir "ping"        $ ping config
        , dir "about"       $ about config
        , dir "hblog"       $ hblog config
        , dir "post"        $ post config
        , dir "files"       $ serveDirectory DisableBrowsing [] "./wwwroot"
        , Main.homePage config
        ]

homePage :: BlogConfig -> ServerPart Response
homePage config = ok $ toResponse $ runReader Pages.Default.homePage config

post :: BlogConfig -> ServerPart Response
post config = path $ \(postId :: String) -> do
    postRecordE <- retrievePageResponse config postId
    case postRecordE of
        Left (sts, msg)         -> generateErrorPage sts msg
        Right postRecord       -> ok $ toResponse $ runReader (postPage postRecord) config

generateErrorPage sts msg = ok $ toResponse $ show sts ++ ":" ++ msg

retrievePageResponse :: BlogConfig -> String -> ServerPart (Either (Int, String) PostRecord)
retrievePageResponse config postId =
    let pId = validatePostId postId in
        case pId of
            Nothing             -> return $ Left (400, "Invalid post ID")
            Just validPostId    -> lift $ getPostFromRepo
                (SqliteRepoDetails "hblog.db")
                validPostId

ping :: BlogConfig -> ServerPart Response
ping _ = ok $ toResponse $ ("OK" :: Text)

about :: BlogConfig -> ServerPart Response
about config = ok $ toResponse $ runReader aboutPage config

hblog :: BlogConfig -> ServerPart Response
hblog config = ok $ toResponse $ runReader aboutHblogPage config
