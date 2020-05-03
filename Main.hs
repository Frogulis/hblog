{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Data.Text (Text)
import Happstack.Lite
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Pages.Post
import Pages.Default (aboutHblogPage, homePage)

serverConfig = defaultServerConfig { port = 8000 }

main :: IO ()
main = do
    putStrLn $ "Running at http://localhost:" ++ (show $ port serverConfig)
    serve (Just serverConfig) myApp

myApp :: ServerPart Response
myApp = msum
    [ dir "ping"        $ ping
    , dir "hblog"       $ hblog
    , dir "post"        $ post
    , Main.homePage
    ]

homePage :: ServerPart Response
homePage = ok $ toResponse $ Pages.Default.homePage

post :: ServerPart Response
post = ok $ toResponse $
    postPage "page title" $ H.p "Body text"

ping :: ServerPart Response
ping = ok $ toResponse $ ("OK" :: Text)

hblog :: ServerPart Response
hblog = ok $ toResponse $ aboutHblogPage
