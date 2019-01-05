{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
-- import           Data.Containers.ListUtils (nubOrdOn)
import           Data.Foldable (for_)
import qualified Data.Set as Set
import qualified Data.Map as Map
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


ordNubBy :: (Ord b) => (a -> b) -> (a -> a -> Bool) -> [a] -> [a]
ordNubBy p f l = go Map.empty l
  where
    go _ []     = []
    go m (x:xs) = let b = p x in case b `Map.lookup` m of
                    Nothing     -> x : go (Map.insert b [x] m) xs
                    Just bucket
                      | elem_by f x bucket -> go m xs
                      | otherwise -> x : go (Map.insert b (x:bucket) m) xs


-- From the Data.List source code.
elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
elem_by _  _ []     = False
elem_by eq y (x:xs) = y `eq` x || elem_by eq y xs


runAnalysis :: IO ()
runAnalysis = do
  let keywords =
        [ "\"virtual reality\""
        , "\"artificial intelligence\""
        ]

  -- First download all API results to build list of all articles
  -- to download before downloading any contents.
  allArticlesWithKeywords <- fmap (ordNubBy fst (==)) . fmap concat $ pooledForConcurrentlyN 4 keywords $ \keyword -> do
    say $ "Downloading articles for keyword: " <> T.pack keyword
    articles <- getArticlesByKeyword keyword
    return [ (a, keyword) | a <- articles ]

  let numArticles = length allArticlesWithKeywords

  -- Now, download all article contents.
  pooledForConcurrentlyN_
    100
    (zip [1..] allArticlesWithKeywords)
    $
    \(i, (a@ApiArticle{ aurl }, keyword)) -> do

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
            , keyword
            ]

