module Pages.Post (postPage) where

import Control.Monad.Trans.Reader

import qualified Text.Blaze.Html5 as H

import BlogConfig
import Repository (PostRecord(..))
import qualified Repository as P (title)
import Pages.Default (pageTemplate)

postPage :: PostRecord -> Reader BlogConfig H.Html
postPage postRecord = postTemplate postRecord >>= pageTemplate (P.title postRecord)

postTemplate :: PostRecord -> Reader BlogConfig H.Html
postTemplate (PostRecord pPostId pTitle content _ publishDate) = return (do
    H.h3 $ H.toHtml pTitle
    H.h4 $ H.toHtml $ maybe "Unpublished -- please report to maintainer" show publishDate
    H.p $ content)
