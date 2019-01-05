{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Downloads where

import           Control.Exception (try, throwIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import           Network.HTTP.Conduit (simpleHttp, HttpException(..), HttpExceptionContent(..), responseStatus)
import           Say (say)
import           System.Directory
import           System.IO
import           Text.Read (readMaybe)
import           Network.HTTP.Types.Status (status404)


-- type alias
type Url = String


makePath :: Url -> FilePath
makePath url = [ if c == '/' then '_' else c | c <- drop 8 url ] ++ ".html"


cachedDownload :: FilePath -> Url -> IO (Maybe ByteString)
cachedDownload cacheDir url = do

  let path = cacheDir ++ "/" ++ makePath url

  exists <- doesFileExist path
  if exists
    then do
      say $ "Cached: " <> T.pack url
      Just <$> BS.readFile path
    else do
      createDirectoryIfMissing True cacheDir
      say $ "Fetching: " <> T.pack url
      eBytes <- try $ simpleHttp url
      case eBytes of
        Left (e :: HttpException) -> case e of
          HttpExceptionRequest _req (StatusCodeException response _bodyBeginning)
            | responseStatus response == status404 -> do
                say $ "Skipping 404-Not-Found URL: " <> T.pack url
                return Nothing
          _ -> throwIO e
        Right bytes -> do
          BSL.writeFile path bytes
          return (Just (BSL.toStrict bytes))
