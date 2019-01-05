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

import           Downloads (makePath, cachedDownload)
import           GuardianApi (getArticlesByKeyword)
import           Scraper (runScraper)
import           Types (ApiArticle(..))


showText :: (Show a) => a -> T.Text
showText = T.pack . show


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

  let numArticles = length allArticles

  -- Now, download all article contents.
  pooledForConcurrentlyN_ 100 (zip [1..] allArticles) $ \(i, ApiArticle{ aurl }) -> do

    say $ "Progress: " <> showText i <> " / " <> showText numArticles
    cachedDownload "data/html" aurl

    -- page <- BS.readFile path
    -- let texts = runScraper page

    -- print texts
