{-# LANGUAGE OverloadedStrings #-}
module Repository where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeLatin1)
import Data.List.Split (splitOn)
import Data.String (IsString)
import Data.Time (UTCTime)

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Text.Blaze.Html5 as H

import qualified Logger as L

data RepoConnDetails = FileRepoDetails { filePath :: String }
                     | SqliteRepoDetails { dbName :: String }

type PostId = String

validatePostId :: String -> Maybe PostId
-- validatePostId postId = case isFormattedCorrectly postId of
--     True            -> Just postId
--     False           -> Nothing
validatePostId = Just . Prelude.id

isFormattedCorrectly :: String -> Bool
isFormattedCorrectly postId =
    let splits = splitOn "-" postId in
        and [ extractNum splits 0 > 0
            , extractNum splits 1 >= 1
            , extractNum splits 1 <= 12
            , extractNum splits 2 >= 1
            , extractNum splits 2 <= 31
            , extractNum splits 3 > 0
            ] 

extractNum arr idx = read (arr !! idx)::Int

data PostRecord = PostRecord { postId           :: PostId
                             , title            :: String
                             , author           :: String
                             , htmlContent      :: H.Html
                             , uploadDate       :: UTCTime
                             , publishDate      :: Maybe UTCTime
                             }

--- sql stuff
data SqlPostRecord = SqlPostRecord { sId                :: Int
                                   , sPostId            :: PostId
                                   , sTitle             :: String
                                   , sAuthor            :: String
                                   , sHtmlContent       :: String
                                   , sUploadDate        :: UTCTime
                                   , sPublishDate       :: Maybe UTCTime
                                   } deriving (Show)

instance FromRow SqlPostRecord where
    fromRow = SqlPostRecord
        <$> field                       -- id
        <*> field                       -- PostId
        <*> field                       -- Title
        <*> field                       -- Author
        <*> field                       -- HtmlContent
        <*> field                       -- UploadDate
        <*> field                       -- PublishDate

toPostRecord :: SqlPostRecord -> PostRecord
toPostRecord (SqlPostRecord _ sPostId sTitle sAuthor sHtmlContent sUploadDate sPublishDate) =
    PostRecord sPostId sTitle sAuthor (H.preEscapedToHtml sHtmlContent) sUploadDate sPublishDate

getPostFromRepo :: RepoConnDetails -> PostId -> IO (Either (Int, String) PostRecord)
getPostFromRepo (SqliteRepoDetails dbName) postId = do
    L.log $ "Retrieving from " ++ dbName ++ " to get post " ++ postId
    conn <- open dbName
    r <- query conn "SELECT Id, PostId, Title, Author, HtmlContent, UploadDate, PublishDate FROM Post WHERE PostId = (?) AND PublishDate IS NOT NULL" (Only postId) :: IO [SqlPostRecord]
    L.log $ "Retrieved " ++ show (length r) ++ " posts"
    close conn
    case r of
        []      -> return $ Left (404,"No values returned")
        x: []   -> return $ Right $ toPostRecord x

getLatestPostFromRepo :: RepoConnDetails -> IO (Either (Int, String) PostRecord)
getLatestPostFromRepo (SqliteRepoDetails dbName) = do
    L.log $ "Retrieving latest post from " ++ dbName
    conn <- open dbName
    r <- query_ conn "SELECT Id, PostId, Title, Author, HtmlContent, UploadDate, PublishDate FROM Post WHERE PublishDate IS NOT NULL ORDER BY PublishDate DESC LIMIT 1" :: IO [SqlPostRecord]
    L.log $ "Retrieved " ++ show (length r) ++ " posts"
    close conn
    case r of
        []      -> return $ Left (404,"No values returned")
        x: []   -> return $ Right $ toPostRecord x