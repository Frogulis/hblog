module Pages.Post (postPage) where

import qualified Text.Blaze.Html5 as H

import Pages.Default (pageTemplate)

postPage :: String -> H.Html -> H.Html
postPage title postBody = pageTemplate title $ postTemplate title postBody

postTemplate :: String -> H.Html -> H.Html
postTemplate title postBody =
    H.div $ do
        H.h3 $ H.toHtml title
        H.hr
        postBody