{-# LANGUAGE OverloadedStrings #-}
module Pages.Default (pageTemplate, errorPage, homePage, aboutPage, aboutHblogPage) where

import Control.Monad.Trans.Reader

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import BlogConfig

pageTemplate :: String -> H.Html -> Reader BlogConfig H.Html
pageTemplate pTitle body = do
    config <- ask
    return (do
        H.html $ do
            H.head $ do
                H.title $ H.toHtml $ title config ++ " - " ++ pTitle
                H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/files/fonts/nunito.css"
                H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/files/styles/lit.css"
                H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/files/styles/custom.css"
            H.body ! A.class_ "c" $ do
                H.a ! A.href "/" $ H.h1 $ H.toHtml $ title config
                header
                H.hr
                H.div $ do
                    body
                )

header = H.div ! A.class_ "row" $ do
    simpleLink "/" "Home"
    simpleLink "/about" "About this blog"
    simpleLink "/posts/latest" "Latest post"
    simpleLink "/hblog" "About hblog"

simpleLink path text = H.div ! A.class_ "col" $ H.a ! A.href path $ text

errorPage :: Int -> String -> Reader BlogConfig H.Html
errorPage sts msg = (return . H.h3) (H.toHtml $ show sts ++ ":" ++ msg) >>= pageTemplate "Error"

homePage :: Reader BlogConfig H.Html
homePage = (return . H.h3) "Welcome to the blog" >>= pageTemplate "Home"

aboutHblogPage :: Reader BlogConfig H.Html
aboutHblogPage = do
    config <- ask
    html <- return (do
        H.h3 "About hblog"
        H.p "hblog is a simple blogging site written entirely in Haskell and CSS"
        H.p $ do
            "On the Haskell side, it uses " 
            H.b "Happstack-lite and Blaze"
            " for templating"
        H.p "It uses the tiny lit.css library, as well as the Nunito font")
    pageTemplate "About hblog" html

aboutPage :: Reader BlogConfig H.Html
aboutPage = do
    config <- ask
    html <- return (do
        H.h3 $ H.toHtml $ "About " ++ title config
        H.p $ H.toHtml $ "Maintained by " ++ maintainer config
        H.p $ H.toHtml $ description config)
    pageTemplate "About this blog" html
