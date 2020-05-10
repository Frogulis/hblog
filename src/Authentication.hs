module Authentication (authenticate) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Data.List.Split (splitOn)
import Data.ByteString (ByteString, stripPrefix)
import Data.ByteString.Char8 (pack)
import Data.Text (unpack)
import Data.Text.Encoding (decodeLatin1)
import Data.ByteString.Base64 (decode)

import Happstack.Lite

import BlogConfig
import Pages.Default (errorPage)
import Repository (validAuth)

authenticate :: BlogConfig -> ServerPart Response -> ServerPart Response
authenticate config successResponse = do
    authStringM <- getHeaderM "Authorization"
    case authStringM of
        Nothing         -> authErrorResponse config "No auth header provided"
        Just bStr       -> withStrippedBasic config successResponse bStr

withStrippedBasic :: BlogConfig -> ServerPart Response -> ByteString -> ServerPart Response
withStrippedBasic config successResponse bStr =
    let strippedM = stripPrefix (pack "Basic ") bStr in
        case strippedM of
            Nothing         -> authErrorResponse config "Require Basic auth"
            Just stripped   -> checkAuthFor config successResponse stripped

checkAuthFor :: BlogConfig -> ServerPart Response -> ByteString -> ServerPart Response
checkAuthFor config successResponse bStr =
    let decodedE = decode bStr in
        case decodedE of
            Left err        -> authErrorResponse config "Invalid auth header"
            Right decStr    -> let auths = (splitOn ":" . unpack . decodeLatin1) decStr in
                case auths of
                    username:password:_ -> guardAuth config successResponse username password
                    _                   -> authErrorResponse config "Invalid auth header"

guardAuth :: BlogConfig -> ServerPart Response -> String -> String -> ServerPart Response
guardAuth config successResponse username password = do
    authResult <- lift $ validAuth (connDetails config) username password
    case authResult of
        True    -> successResponse
        False   -> authErrorResponse config "Not authorized"

authErrorResponse :: BlogConfig -> String -> ServerPart Response
authErrorResponse config msg = unauthorized $ toResponse $ runReader (errorPage 401 msg) config
