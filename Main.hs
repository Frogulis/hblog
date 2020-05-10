{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Data.Text (Text)

import Happstack.Lite
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Authentication (authenticate)
import BlogConfig
import Pages.Post
import Pages.Default (errorPage, aboutPage, aboutHblogPage, homePage)
import Repository

serverConfig = defaultServerConfig { port = 8000 }

blogConfig = BlogConfig
    "Sample blog"
    "Jamie Hoffmann"
    "A sample blog about something blah blah.\nWhatever and ever!"
    (SqliteRepoDetails "hblog.db")


main :: IO ()
main = do
    putStrLn $ "Running at http://localhost:" ++ (show $ port serverConfig)
    serve (Just serverConfig) (myApp blogConfig)

-- load in config before this point so that it can be hot-reloaded while
-- separate from rest of application
myApp :: BlogConfig -> ServerPart Response
myApp config = do
    msum
        [ method POST >> msum
            [ dir "posts"       $ authenticate config $ uploadPost config ]
        , method GET >> msum
            [ dir "ping"        $ ping config
            , dir "about"       $ about config
            , dir "hblog"       $ hblog config
            , dir "posts"        $ post config
            , dir "files"       $ serveDirectory DisableBrowsing [] "./wwwroot"
            , Main.homePage config
            ]
        ]

uploadPost :: BlogConfig -> ServerPart Response
uploadPost config = ok $ toResponse $ ("Authenticated!" :: String)

homePage :: BlogConfig -> ServerPart Response
homePage config = ok $ toResponse $ runReader Pages.Default.homePage config

post :: BlogConfig -> ServerPart Response
post config = path $ \(postId :: String) -> do
    postRecordE <- retrievePageResponse config postId
    case postRecordE of
        Left (sts, msg)        -> generateErrorPage config sts msg
        Right postRecord       -> ok $ toResponse $ runReader (postPage postRecord) config

generateErrorPage :: BlogConfig -> Int -> String -> ServerPart Response
generateErrorPage config sts msg = ok $ toResponse $ runReader (errorPage sts msg) config

retrievePageResponse :: BlogConfig -> String -> ServerPart (Either (Int, String) PostRecord)
retrievePageResponse config postPath =
    case postPath of
        "latest"        -> retrieveLatestPostResponse config
        _               -> retrieveSpecificPostResponse config postPath

retrieveLatestPostResponse :: BlogConfig -> ServerPart (Either (Int, String) PostRecord)
retrieveLatestPostResponse config = lift $ getLatestPostFromRepo (connDetails config)

retrieveSpecificPostResponse :: BlogConfig -> String -> ServerPart (Either (Int, String) PostRecord)
retrieveSpecificPostResponse config postId =
    let pId = validatePostId postId in
        case pId of
            Nothing             -> return $ Left (400, "Invalid post ID")
            Just validPostId    -> lift $ getPostFromRepo
                (connDetails config)
                validPostId

ping :: BlogConfig -> ServerPart Response
ping _ = ok $ toResponse $ ("OK" :: Text)

about :: BlogConfig -> ServerPart Response
about config = ok $ toResponse $ runReader aboutPage config

hblog :: BlogConfig -> ServerPart Response
hblog config = ok $ toResponse $ runReader aboutHblogPage config
