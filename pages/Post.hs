module Pages.Post (postPage) where

import Control.Monad.Trans.Reader

import qualified Text.Blaze.Html5 as H

import BlogConfig
import Pages.Default (pageTemplate)

postPage :: String -> H.Html -> Reader BlogConfig H.Html
postPage title postBody = postTemplate title postBody >>= pageTemplate title

postTemplate :: String -> H.Html -> Reader BlogConfig H.Html
postTemplate title postBody = return (do
    H.h3 $ H.toHtml title
    postBody)
