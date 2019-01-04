module HoH.LoadArticles where

import Text.Read (readMaybe)
import System.IO
import System.Directory
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BL

-- the URL we're going to search
myurl = "https://www.theguardian.com/games/2018/dec/11/the-11-best-games-on-playstation-vr"

-- Alternative:
-- output = L.putStrLn . L.take 500 =<< simpleHttp url


downloadHtml :: String -> IO ()
downloadHtml url = do
    bytes <- simpleHttp url
    -- L.putStrLn (L.take 500 bytes)
    BSL8.putStrLn (bytes)
    createDirectoryIfMissing True "data/html"
    handle <- openFile "data/html/playstation.html" WriteMode
    BL.hPut handle bytes
    hClose handle

parseHtml = do
    undefined
