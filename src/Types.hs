module Types where


data ApiArticle = ApiArticle
  { aid        :: String
  , atitle     :: String
  , adate      :: String
  , asection   :: String
  , aurl       :: String
  } deriving (Eq, Ord, Show, Read)


data ArticleHtml = ArticleHtml
  { articleHtmlId :: String
  , html :: String
  }

{-
  A guardian article
-}
-- data Article = Article
--   { id        :: String
--   , title     :: String
--   , text      :: String
--   , date      :: String
--   , section   :: String
--   , url       :: String
--   } deriving (Show, Read)
