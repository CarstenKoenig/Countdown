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

render :: State -> Html
render _ = html $ do
  script ! A.src "jquery.js" $ text ""
  script ! A.src "knockout.js" $ text ""
  script ! A.src "admin.js" $ text ""
  body $ do
    H.div ! A.class_ "Main" $ do
      h1 "COUNTdown"
      H.button ! dataBind "click: startRound, enable: canStart" $ "starten.."
      H.div ! A.class_ "Aufgabe" $ do
        p $ do
          H.span "Ziel: "
          H.span ! dataBind "text: target" $ ""
        p $ do
          H.span "Zahlen: "
          H.span ! dataBind "text: numbers" $ ""
      H.div ! A.class_ "Ergebnisse" $ do
        H.table $ do
          H.thead $ do
            H.td "Spieler"
            H.td "Diff."
          H.tr ! dataBind "foreach: scores" $ do
            H.td ! dataBind "text: name" $ ""
            H.td ! dataBind "text: diff" $ ""
      
dataBind :: AttributeValue -> Attribute
dataBind = dataAttribute "bind"
