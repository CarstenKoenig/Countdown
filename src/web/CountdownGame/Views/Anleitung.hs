{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Anleitung where

import Data.Text (pack)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM_)

import CountdownGame.Spiel (State)

render :: Html
render = html $ do
  script ! A.src "jquery.js" $ text ""
  script ! A.src "jquery.timer.js" $ text ""
  body $ do

    H.header $ do
      H.h1 "CountDown"

    H.section $ do

      H.h2 "Anleitung"

      H.p ""

      H.br

      H.a ! A.href "/play" $ "Spielen"

    H.footer "Developer Open Space 2015"
      
dataBind :: AttributeValue -> Attribute
dataBind = dataAttribute "bind"
