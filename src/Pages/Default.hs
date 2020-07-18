{-# LANGUAGE OverloadedStrings #-}
module Pages.Default (pageTemplate, errorPage, homePage, homePageWithPost, aboutPage, aboutHblogPage, archivePage) where

import Control.Monad.Trans.Reader

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import BlogConfig
import qualified BlogConfig as B (title)
import Pages.Post (postTemplate)
import Pages.Template
import Repository (PostId, PostRecord(..))

errorPage :: Int -> String -> Reader BlogConfig H.Html
errorPage sts msg = (return . H.h3) (H.toHtml $ show sts ++ ":" ++ msg) >>= pageTemplate 0 "Error"

homePage :: Reader BlogConfig H.Html
homePage = do
    html <- return (do
        H.h3 "Welcome to the blog"
        H.p "Watch this space for posts!")
    pageTemplate 0 "Home" html

homePageWithPost :: PostRecord -> Reader BlogConfig H.Html
homePageWithPost postRecord = do
    post <- postTemplate postRecord
    html <- return (do
        H.h3 "Welcome to the blog"
        post)
    pageTemplate 0 "Home" html

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
    pageTemplate 0 "About hblog" html

aboutPage :: Reader BlogConfig H.Html
aboutPage = do
    config <- ask
    html <- return (do
        H.h3 $ H.toHtml $ "About " ++ B.title config
        H.p $ H.toHtml $ "Maintained by " ++ maintainer config
        H.p $ H.toHtml $ description config)
    pageTemplate 0 "About this blog" html

archivePage :: Int -> (Int, [(PostId, String)]) -> Reader BlogConfig H.Html
archivePage pageNo (totalPages, records) = do
    config <- ask
    html <- return (do
        H.h3 "Archive"
        pageButtons totalPages pageNo
        archiveList records
        pageButtons totalPages pageNo)
    pageTemplate 1 "Archive" html

archiveList :: [(PostId, String)] -> H.Html
archiveList records = case records of
    []      -> "No posts available"
    _       -> do
        H.table $ do
            H.toHtml $ (map rowify records)
    where
        rowify (pId, pTitle) = do
            H.tr $ do
                H.td $ do
                    H.a ! A.href (H.toValue $ "../posts/" ++ pId) $ H.toHtml pTitle

pageButtons totalPages pageNo = case totalPages of
    1   -> H.div ""
    _   -> H.p $ do
        pageButton (pageNo - 1) (not $ pageNo == 0) "Back"
        H.span $ " | "
        pageButton (pageNo + 1) (pageNo + 1 < totalPages) "Forward"

pageButton num active text = case active of
    True    -> H.a ! A.href (H.toValue $ "../archive/" ++ show num) $ text
    False   -> H.span ! A.class_ "inactive" $ text