{-# LANGUAGE OverloadedStrings #-}
module Repository where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Hashable (hashWithSalt)
import Data.Text.Encoding (decodeLatin1)
import Data.List.Split (splitOn)
import Data.String (IsString)
import Data.Time (UTCTime)

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Text.Blaze.Html5 as H

import BlogConfig (RepoConnDetails(..))
import qualified Logger as L

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

getAnyPostFromRepo :: RepoConnDetails -> PostId -> IO (Either (Int, String) PostRecord)
getAnyPostFromRepo (SqliteRepoDetails dbName) postId = do
    L.log $ "Retrieving from " ++ dbName ++ " to get post " ++ postId
    conn <- open dbName
    r <- query conn "SELECT Id, PostId, Title, Author, HtmlContent, UploadDate, PublishDate FROM Post WHERE PostId = (?)" (Only postId) :: IO [SqlPostRecord]
    L.log $ "Retrieved " ++ show (length r) ++ " posts"
    close conn
    case r of
        []      -> return $ Left (404,"No values returned")
        x: []   -> return $ Right $ toPostRecord x

-- pageNo is 0-indexed
getArchivePageFromRepo :: RepoConnDetails -> Int -> Int -> IO (Int, [(PostId, String)])
getArchivePageFromRepo (SqliteRepoDetails dbName) pageSize pageNo = do
    L.log $ "Retrieving page number " ++ show pageNo ++ " of post archive"
    conn <- open dbName
    records <- query conn
        "SELECT PostId, Title FROM Post WHERE PublishDate IS NOT NULL ORDER BY PublishDate DESC LIMIT (?) OFFSET (?)"
        (pageSize, pageNo * pageSize)
        :: IO [(PostId, String)]
    counts <- query_ conn
        "SELECT count(PostId) FROM Post WHERE PublishDate IS NOT NULL"
        :: IO [Only Int]
    L.log $ "Retrieved " ++ (show . length) records ++ " posts"
    close conn
    return (getTotalPages pageSize ((fromOnly . head) counts), records)

getTotalPages :: Int -> Int -> Int
getTotalPages pageSize totalPosts =
    totalPosts `div` pageSize + (if totalPosts `rem` pageSize > 0 then 1 else 0)

publishPostInRepo :: RepoConnDetails -> PostId -> UTCTime -> IO ()
publishPostInRepo (SqliteRepoDetails dbName) postId publishDate = do
    L.log $ "Publish in " ++ dbName ++ " for post " ++ postId
    conn <- open dbName
    r <- execute conn "UPDATE Post SET PublishDate = (?) WHERE PostId = (?)" (publishDate, postId)
    close conn
    L.log $ "Published"

validAuth :: RepoConnDetails -> String -> String -> IO Bool
validAuth (SqliteRepoDetails dbName) username password = do
    L.log $ "Checking auth from " ++ dbName ++ " for " ++ username
    conn <- open dbName
    r <- query conn "SELECT PasswordHash, HashSalt FROM User WHERE Username = (?)" (Only username) :: IO [(String, Int)]
    close conn
    case r of
        []                  -> return False
        (pHash, salt): []   -> let tryHash = show $ hashWithSalt salt password in
            return $ pHash == tryHash
