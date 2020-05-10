module Logger where

import Data.Time

log :: String -> IO ()
log str = do
    time <- getCurrentTime
    putStrLn $ "[" ++ show time ++ "]" ++ str
