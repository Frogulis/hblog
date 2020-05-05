module Repository where

import Data.List.Split (splitOn)
import qualified Text.Blaze.Html5 as H

data RepoConnDetails = FileRepoDetails String

type PostId = String

validatePostId :: String -> Maybe PostId
validatePostId postId = case isFormattedCorrectly postId of
    True            -> Just postId
    False           -> Nothing

isFormattedCorrectly postId =
    let splits = splitOn '-' postId in
        and [ extractNum splits 0 > 0
            , extractNum splits 1 >= 1
            , extractNum splits 1 <= 12
            , extractNum splits 2 >= 1
            , extractNum splits 2 <= 31
            , extractNum splits 3 > 0
            ] 

extractNum arr idx = read (arr !! idx)::Int

getPostFromRepo :: RepoConnDetails -> String -> H.Html
getPostFromRepo = undefined
