{-# LANGUAGE OverloadedStrings #-}
module Pages.Template (pageTemplate) where

import Control.Monad.Trans.Reader

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import BlogConfig
import qualified BlogConfig as B (title)

pageTemplate :: Int -> String -> H.Html -> Reader BlogConfig H.Html
pageTemplate depth pTitle body = do
    config <- ask
    return (do
        H.html $ do
            H.head $ do
                H.title $ H.toHtml $ B.title config ++ " - " ++ pTitle
                H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (deepen depth "files/fonts/nunito.css")
                H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (deepen depth "files/styles/lit.css")
                H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (deepen depth "files/styles/custom.css")
            H.body ! A.class_ "c" $ do
                H.a ! A.href (deepen depth "home") $ H.h1 $ H.toHtml $ B.title config
                header depth
                H.hr
                H.div $ do
                    body
                )

deepen depth link =
    H.toValue $ concat (replicate depth "../") ++ link

header depth = H.div ! A.class_ "row" $ do
    simpleLink (deepen depth "posts/latest") "Latest post"
    simpleLink (deepen depth "archive/0") "Archive"
    simpleLink (deepen depth "about") "About this blog"

simpleLink path text = H.div ! A.class_ "col" $ H.a ! A.href path $ text
