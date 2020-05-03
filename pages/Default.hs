{-# LANGUAGE OverloadedStrings #-}
module Pages.Default (pageTemplate, homePage, aboutHblogPage) where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

pageTemplate :: String -> H.Html -> H.Html
pageTemplate title body =
    H.html $ do
        H.head $ do
            H.title $ H.toHtml title
        H.body $ do
            body
            H.hr
            footer

footer = H.div ! A.class_ "footer" $ do
    simpleLink "/" "Home"
    simpleLink "/post" "A post!"
    simpleLink "/hblog" "About hblog"

simpleLink path text = H.p $ H.a ! A.href path $ text

homePage :: H.Html
homePage = pageTemplate "Home" $ do
    H.p "Welcome to the blog"

aboutHblogPage :: H.Html
aboutHblogPage = pageTemplate "About hblog" $ do
    H.p "Something about hblog"