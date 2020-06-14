{-# LANGUAGE OverloadedStrings #-}
module UploadRepository where

import Database.SQLite.Simple
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String (renderHtml)

import BlogConfig (RepoConnDetails(..))
import qualified Logger as L
import Repository (PostRecord(..), PostId)

instance ToRow PostRecord where
    toRow record =
        toRow ( postId record
              , title record
              , author record
              , renderHtml $ htmlContent record
              , uploadDate record
              , publishDate record )

uploadPostToRepo :: RepoConnDetails -> PostRecord -> IO Bool
uploadPostToRepo (SqliteRepoDetails dbName) args = do
    L.log $ "Uploading to " ++ dbName ++ " post with ID " ++ postId args
    conn <- open dbName
    execute conn "INSERT INTO Post VALUES ((select max(id) + 1 from post), (?), (?), (?), (?), (?), (?));" args
    L.log $ "Uploaded post with ID " ++ postId args
    close conn
    return True
