{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import           Control.Concurrent.Async
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Say (say)
import           System.Directory (doesFileExist)
import           UnliftIO.Async (pooledForConcurrentlyN_)

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

  pooledForConcurrentlyN_ 4 keywords $ \keywords -> do
    articleList <- getArticlesByKeyword keywords

    pooledForConcurrentlyN_ 100 articleList $ \ApiArticle{ aurl } -> do

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
