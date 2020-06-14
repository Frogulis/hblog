{-# LANGUAGE OverloadedStrings #-}
module Pages.Post (postPage) where

import Control.Monad.Trans.Reader
import Data.Time.Format (formatTime, defaultTimeLocale)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import BlogConfig
import Repository (PostRecord(..))
import qualified Repository as P (title)
import Pages.Default (pageTemplate)

postPage :: PostRecord -> Reader BlogConfig H.Html
postPage postRecord = postTemplate postRecord >>= pageTemplate (P.title postRecord)

postTemplate :: PostRecord -> Reader BlogConfig H.Html
postTemplate (PostRecord pPostId pTitle author content _ publishDate) = return (do
    H.div $ do
        H.h3 $ H.toHtml pTitle
        H.h5 $ H.toHtml author
        H.h5 $ H.toHtml $ maybe "Unpublished -- please report to maintainer" getDatestring publishDate
    H.p $ content)
    where
        getDatestring = formatTime defaultTimeLocale "%F"
