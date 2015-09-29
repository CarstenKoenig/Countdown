{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Admin where

import Data.Text (pack)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM_)

import CountdownGame.Game
import qualified CountdownGame.Game as G

render :: [Player] -> Maybe Round -> Html
render players nextRound =
  html . body $ do
      h1 "COUNTdown"
      h3 "Mitspieler"
      ul . forM_ players $ \player ->
        li . text  $ nickName player
      case nextRound of
        Just round -> do
          h3 "Runde"
          p . text . pack $ show (G.target $ G.params round)
          p . text . pack $ show (G.numbers $ G.params round)
        Nothing ->
          h3 "Warte..."
