module Header (getMultipleHeaders) where

import Happstack.Lite (ServerPart, getHeaderM)
import Data.ByteString (ByteString)

getMultipleHeaders :: [String] -> ServerPart (Either String [(String, ByteString)])
getMultipleHeaders headers = (fmap (fmap (zip headers)) . fmap sequence . sequence . map getHeaderE) headers

getHeaderE :: String -> ServerPart (Either String ByteString)
getHeaderE str = do
    out <- getHeaderM str
    case out of
        Nothing     -> return $ Left str
        Just out    -> return $ Right out
