module HoH.Types where

-- import Date.Dates (DateTime)

data ArticleUrl = ArticleUrl {
  id :: String,
  url :: String,
}

type ArticleUrls = [ArticleUrl]

data ArticleHtml = ArticleHtml {
  id :: String,
  html :: String,
}

{-
  A guardian article
-}
data Article = Article {
  id        :: String,
  title     :: String,
  text      :: String,
  date      :: String,
  section   :: String,
  url       :: String
} deriving (Show, Read)
