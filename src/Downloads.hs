{-# LANGUAGE OverloadedStrings #-}

module Downloads where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import           Network.HTTP.Conduit (simpleHttp)
import           Say (say)
import           System.Directory
import           System.IO
import           Text.Read (readMaybe)


-- type alias
type Url = String


makePath :: Url -> FilePath
makePath url = [ if c == '/' then '_' else c | c <- drop 8 url ] ++ ".html"


cachedDownload :: FilePath -> Url -> IO ()
cachedDownload cacheDir url = do

  let path = cacheDir ++ "/" ++ makePath url

  exists <- doesFileExist path
  if exists
    then do
      say $ "Cached: " <> T.pack url
    else do
      createDirectoryIfMissing True cacheDir
      say $ "Fetching: " <> T.pack url
      bytes <- simpleHttp url
      BSL.writeFile path bytes
