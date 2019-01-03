module HoH.LoadArticles where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L

-- the URL we're going to search
url = "http://www.bing.com/search?q=school+of+haskell"

-- test
main = L.putStrLn . L.take 500 =<< simpleHttp url