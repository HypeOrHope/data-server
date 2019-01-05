{-# LANGUAGE OverloadedStrings #-}
module Server where

import Web.Scotty

import Data.Monoid (mconcat)

serverMain = scotty 3000 $
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1 style=\"color: red\">Scotty, ", beam, " me up!</h1>"]