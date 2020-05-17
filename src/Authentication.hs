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
    let auths = maybeToEither "No auth header provided" authStringM >>= stripBasicE >>= decodeAuthE >>= splitAuthE in
        case auths of
            Left str                    -> authErrorResponse config str
            Right (username, password)  -> guardAuth config successResponse username password

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither errorVal Nothing  = Left errorVal
maybeToEither _ (Just x)        = Right x

stripBasicE :: ByteString -> Either String ByteString
stripBasicE bStr =
    let strippedM = stripPrefix (pack "Basic ") bStr in
        case strippedM of
            Nothing         -> Left "Require Basic auth"
            Just stripped   -> Right stripped

decodeAuthE :: ByteString -> Either String ByteString
decodeAuthE bStr =
    let decodedE = decode bStr in
        case decodedE of
            Left err        -> Left "Invalid auth header encoding"
            Right decStr    -> Right decStr

splitAuthE :: ByteString -> Either String (String, String)
splitAuthE str =
    let auths = (splitOn ":" . unpack . decodeLatin1) str in
        case auths of
            username:password:_ -> Right (username, password)
            _                   -> Left "Invalid auth header"

guardAuth :: BlogConfig -> ServerPart Response -> String -> String -> ServerPart Response
guardAuth config successResponse username password = do
    authResult <- lift $ validAuth (connDetails config) username password
    case authResult of
        True    -> successResponse
        False   -> authErrorResponse config "Not authorized"

authErrorResponse :: BlogConfig -> String -> ServerPart Response
authErrorResponse config msg = unauthorized $ toResponse $ runReader (errorPage 401 msg) config
