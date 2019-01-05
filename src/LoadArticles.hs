module LoadArticles where

import Text.Read (readMaybe)
import System.IO
import System.Directory
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BL


-- type alias
type Url = String

-- the URL we're going to search
myurl :: Url
myurl = "https://www.theguardian.com/games/2018/dec/11/the-11-best-games-on-playstation-vr"
url2 = "https://www.theguardian.com/sport/2019/jan/04/cash-dispute-could-see-catalan-dragons-not-defend-challenge-cup-crown-rugby-league"

makePath :: Url -> FilePath
makePath url = ("data/html/" ++ [ if c == '/' then '_' else c | c <- drop 8 url ] ++ ".html")

downloadHtml :: Url -> IO ()
downloadHtml url = do
    bytes <- simpleHttp url
    createDirectoryIfMissing True "data/html"
    let path = makePath url
    handle <- openFile path WriteMode
    BL.hPut handle bytes
    hClose handle

parseHtml = do
    undefined
