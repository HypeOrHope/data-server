{-# LANGUAGE OverloadedStrings #-}
module HoH.Server where

import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1 style=\"color: red\">Scotty, ", beam, " me up!</h1>"]