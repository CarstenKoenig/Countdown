{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Play where

import Data.Text (append)
import Data.Text.Lazy (toStrict)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import CountdownGame.Game

render :: Player -> Html
render player = do
  html $ do
    body $ do
      h1 "COUNTdown"
      h3 . text $  "Hallo " `append` (toStrict $ nickName player)
      ul $ do
        li "Player"
