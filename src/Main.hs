{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Text (Text, unpack)
import qualified Data.Text.Internal.Lazy as TL (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Time (UTCTime, getCurrentTime)

import Happstack.Lite
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Authentication (authenticate)
import BlogConfig
import Header
import Pages.Post
import Pages.Default (errorPage, aboutPage, aboutHblogPage, homePage)
import Repository
import UploadRepository

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
            [ dir "posts"       $ authenticate config $ uploadPost config
            , dir "publish"     $ authenticate config $ publishPost config ]
        , method GET >> msum
            [ dir "ping"        $ ping config
            , dir "about"       $ about config
            , dir "hblog"       $ hblog config
            , dir "posts"       $ post config
            , dir "unpublished" $ authenticate config $ unpublished config
            , dir "files"       $ serveDirectory DisableBrowsing [] "./wwwroot"
            , Main.homePage config
            ]
        ]

publishPost :: BlogConfig -> ServerPart Response
publishPost config = path $ \(postId :: String) -> do
    case validatePostId postId of
        Just pId    -> do
            time <- lift getCurrentTime
            lift $ publishPostInRepo (connDetails config) pId time
            ok $ toResponse $ ("published"::String)
        Nothing     -> setResponseCode 400 >> return (toResponse $ ("not published"::String))

uploadPost :: BlogConfig -> ServerPart Response
uploadPost config = do
    metadata <- getMultipleHeaders ["PostId", "PostTitle", "PostAuthor", "Publish"]
    htmlContent <- (lookText "htmlcontent")
    time <- lift getCurrentTime
    case metadata of
        Left header     -> setResponseCode 400 >> (generateErrorPage config 400 $ "Can't find header " ++ header)
        Right values    -> do
            result <- lift $ uploadPostToRepo (connDetails config) (buildPostRecord values time htmlContent)
            case result of
                True    -> ok $ toResponse $ ("posted"::String)
                False   -> internalServerError $ toResponse $ ("error while posting"::String)

buildPostRecord :: [(String, ByteString)] -> UTCTime -> TL.Text -> PostRecord
buildPostRecord metadata nowDate htmlContent = PostRecord
    (extract "PostId" metadata)
    (extract "PostTitle" metadata)
    (extract "PostAuthor" metadata)
    (H.preEscapedToHtml htmlContent)
    (nowDate)
    (if (extract "Publish" metadata == "1") then Just nowDate else Nothing)
    where
        extract s = (unpack . decodeLatin1 . snd . head . filter (\x -> fst x == s))

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

unpublished :: BlogConfig -> ServerPart Response
unpublished config = path $ \(postId :: String) -> do
    postRecordE <- unpublishedPageResponse config postId
    case postRecordE of
        Left (sts, msg)     -> generateErrorPage config sts msg
        Right postRecord    -> ok $ toResponse $ runReader (postPage postRecord) config

unpublishedPageResponse :: BlogConfig -> String -> ServerPart (Either (Int, String) PostRecord)
unpublishedPageResponse config postId =
    let pId = validatePostId postId in
        case pId of
            Nothing             -> return $ Left (400, "Invalid post ID")
            Just validPostId    -> lift $ getAnyPostFromRepo
                (connDetails config)
                validPostId

ping :: BlogConfig -> ServerPart Response
ping _ = ok $ toResponse $ ("OK" :: Text)

about :: BlogConfig -> ServerPart Response
about config = ok $ toResponse $ runReader aboutPage config

hblog :: BlogConfig -> ServerPart Response
hblog config = ok $ toResponse $ runReader aboutHblogPage config
