{-# LANGUAGE OverloadedStrings #-}  -- Allow Strings for other objects similar to strings

module Scraper where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Network.HTTP.Conduit (simpleHttp)
import           System.IO
import           Text.HTML.DOM (parseLBS)
import           Text.Read (readMaybe)
import           Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, ($.//), ($//), (&|), (&//), (>=>), checkName)

import qualified Data.ByteString.Lazy.Char8 as L

-- the URL we're going to search
url = "https://www.theguardian.com/games/2018/dec/11/the-11-best-games-on-playstation-vr"

cursorFor :: String -> IO Cursor
cursorFor url = do
     page <- simpleHttp url
     -- page <- BSL.readFile "/home/....html"
     return $ fromDocument $ parseLBS page

getName :: Cursor -> [T.Text]
getName cursor = cursor
  $// element "div"
    >=> attributeIs "class" "content__article-body from-content-api js-article__body"
    >=> child
    >=> checkName (`elem` ["p", "h2"])
  &// content

runScraper :: ByteString -> [T.Text]
runScraper page =
  let cursor = fromDocument $ parseLBS (BSL.fromStrict page)
  in getName cursor

load = do
     cursor <- cursorFor url
     print (getName cursor)

