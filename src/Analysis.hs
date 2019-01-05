{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Say (say)
import           System.Directory (doesFileExist)
import           UnliftIO.Async (pooledForConcurrentlyN, pooledForConcurrentlyN_)

import GuardianApi (getArticlesByKeyword)
import LoadArticles (makePath, downloadHtml)
import Scraper (runScraper)
import Types (ApiArticle(..))


runAnalysis :: IO ()
runAnalysis = do
  let keywords =
        [ "vr"
        , "artificial intelligence"
        ]

  -- First download all API results to build list of all articles
  -- to download before downloading any contents.
  allArticles <- fmap concat $ pooledForConcurrentlyN 4 keywords $ \keyword -> do
    say $ "Downloading articles for keyword: " <> T.pack keyword
    getArticlesByKeyword keyword

  -- Now, download all article contents.
  pooledForConcurrentlyN_ 100 allArticles $ \ApiArticle{ aurl } -> do

    let path = makePath aurl
    exists <- doesFileExist path
    if exists
      then say $ "Using from cache: " <> T.pack aurl
      else do
        say $ "Fetching: " <> T.pack aurl
        downloadHtml aurl

    -- page <- BS.readFile path
    -- let texts = runScraper page

    -- print texts
