{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Admin where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM_)

import CountdownGame.Game

render :: [Player] -> Html
render players = do
  html $ do
    body $ do
      h1 "COUNTdown"
      h3 "Mitspieler"
      ul . forM_ players $ \player ->
        li . text  $ nickName player
