{-# LANGUAGE OverloadedStrings #-}

module GuardianApi where

import Control.Monad
import Control.Monad.IO.Class
import Data.Traversable (for)
import Network.Guardian.ContentApi
import Network.Guardian.ContentApi.Content
import Network.Guardian.ContentApi.URL
import Network.Guardian.ContentApi.Section -- für section
import Network.Guardian.ContentApi.URL -- für unURL
import UnliftIO.Async (pooledForConcurrentlyN)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text


import Types



------------- Daten einlesen und als IO [Content] speichern --------------------------------
downloadPageSize :: Int
downloadPageSize = 200

hohapiKey :: Text.Text
hohapiKey = "test"


getAll :: Text.Text -> IO [Content]
getAll keyword = do
  response <- callApi keyword 1
  let firstPageContentsList = results response
  let numberOfPages = callcCallNumber (totalResults response) downloadPageSize

  listOfListOfContents <- pooledForConcurrentlyN 100 [2..numberOfPages] $ \pageNo -> do
    response <- callApi keyword pageNo
    return (results response)

  let listOfContents = concat (firstPageContentsList:listOfListOfContents)
  return listOfContents

callApi :: Text.Text -> Int -> IO ContentSearchResult
callApi keyword p = do
  config <- defaultApiConfig (Just hohapiKey)
  runContentApi (config) $ contentSearchExt (query) (Just downloadPageSize) (Just p)
  where
    query = ContentSearchQuery (Just keyword) [] []

callcCallNumber :: Int -> Int -> Int
callcCallNumber a b
  | a `mod` b /= 0 = 1 + div a b
  | otherwise = div a b
------------- Ende Daten einlesen  --------------------------------

------------- Daten formatieren. Von IO [Content] -> [Article]------
hohFormat :: Content -> ApiArticle
hohFormat c = ApiArticle
  {
    aid = Text.unpack $ unContentId (contentId c)
  , atitle = Text.unpack $ webTitle c
  , adate  = show $ webPublicationDate c
  , asection = getSection (section c)
  , aurl =  Text.unpack (unURL (webUrl c))
  }

getSection :: Maybe Section -> String
getSection sec = case sec of
  Just s -> Text.unpack (name s)
  Nothing ->  ""

printArticle :: ApiArticle -> IO  ()
printArticle art = do
  _ <- putStrLn $ "Id: " ++ (aid art)
  _ <- putStrLn $ "Title: " ++ (atitle art)
  _ <- putStrLn $ "Date : " ++ (adate art)
  _ <- putStrLn $ "Section: " ++ (asection art)
  _ <- putStrLn $ "Url: " ++ (aurl art)
  _ <- putStrLn  " "
  pure ()
------------- Ende Daten formatieren. Von IO [Content] -> [Article]------

------------- Volständige Funktion zum Einlesen und Parsen--------------


getArticlesByKeyword :: String -> IO [ApiArticle]
getArticlesByKeyword key = do
  contents <- getAll (Text.pack key)
  let arts =  map hohFormat contents
  pure arts


testApi :: IO ()
testApi = do
  lst <- getAll "VR"
  let arts =  map hohFormat lst
  mapM_ printArticle arts
