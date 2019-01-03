module HoH.LoadArticles where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L

-- the URL we're going to search
url = "https://www.theguardian.com/games/2018/dec/11/the-11-best-games-on-playstation-vr"

-- Alternative:
-- output = L.putStrLn . L.take 500 =<< simpleHttp url

output = do
    bytes <- simpleHttp url
    -- L.putStrLn (L.take 500 bytes)
    L.putStrLn (bytes)

