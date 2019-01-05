{-# LANGUAGE NamedFieldPuns #-}

module Analysis where

import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import qualified Data.ByteString as BS
import           System.Directory (doesFileExist)

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

  for_ keywords $ \keywords -> do
    articleList <- getArticlesByKeyword keywords

    for_ articleList $ \ApiArticle{ aurl } -> do

      let path = makePath aurl
      exists <- doesFileExist path
      if exists
        then putStrLn $ "Using from cache: " ++ aurl
        else do
          putStrLn $ "Fetching: " ++ aurl
          downloadHtml aurl

      -- page <- BS.readFile path
      -- let texts = runScraper page

      -- print texts
