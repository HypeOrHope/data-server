{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Say (say)
import           System.FilePath (takeDirectory)
import           System.Directory (doesFileExist, createDirectoryIfMissing)
import           UnliftIO.Async (pooledForConcurrentlyN, pooledForConcurrentlyN_)

import           Downloads (makePath, cachedDownload)
import           GuardianApi (getArticlesByKeyword)
import           Scraper (runScraper)
import           Types (ApiArticle(..))


showText :: (Show a) => a -> T.Text
showText = T.pack . show


deduplicate :: (Ord a) => [a] -> [a]
deduplicate = Set.toList . Set.fromList


runAnalysis :: IO ()
runAnalysis = do
  let keywords =
        [ "\"virtual reality\""
        , "\"artificial intelligence\""
        ]

  -- First download all API results to build list of all articles
  -- to download before downloading any contents.
  allArticles <- fmap deduplicate . fmap concat $ pooledForConcurrentlyN 4 keywords $ \keyword -> do
    say $ "Downloading articles for keyword: " <> T.pack keyword
    getArticlesByKeyword keyword

  let numArticles = length allArticles

  -- Now, download all article contents.
  pooledForConcurrentlyN_ 100 (zip [1..] allArticles) $ \(i, a@ApiArticle{ aurl }) -> do

    say $ "Progress: " <> showText i <> " / " <> showText numArticles
    mPage <- cachedDownload "data/html" aurl

    case mPage of
      Nothing -> return ()
      Just page -> do
        let texts = runScraper page

        let text = T.unlines texts

        let path = "data/text/" ++ makePath aurl ++ ".txt"
        createDirectoryIfMissing True (takeDirectory path)
        T.writeFile path text


        let ApiArticle{ atitle, adate, asection } = a

        let metaPath = "data/meta/" ++ makePath aurl ++ ".txt"
        createDirectoryIfMissing True (takeDirectory metaPath)
        writeFile metaPath $ unlines
          [ atitle
          , adate
          , asection
          ]

