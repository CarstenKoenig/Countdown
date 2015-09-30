{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Admin where

import Data.Text (pack)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM_)

import CountdownGame.State

render :: State -> Html
render _ = html $ do
  script ! A.src "jquery.js" $ text ""
  script ! A.src "jquery.timer.js" $ text ""
  script ! A.src "knockout.js" $ text ""
  script ! A.src "admin.js" $ text ""
  body $ H.div ! A.class_ "Main" $ do
      h1 "COUNTdown"
      H.button ! dataBind "click: startRound, enable: isStartable" $ "starten.."
      H.div ! A.class_ "Aufgabe" $ do
        p $ do
          H.span "Ziel: "
          H.span ! dataBind "text: goal" $ ""
        p $ do
          H.span "Zahlen: "
          H.span ! dataBind "text: numbers" $ ""
        H.div ! A.class_ "Countdown" ! dataBind "visible: isRunning" $ do
          p ! dataBind "text: secondsLeft" $ ""
      H.div ! A.class_ "Ergebnisse" $ do
        H.table $ do
          H.thead $ do
            H.td "Spieler"
            H.td "Diff."
            H.td "Formel"
          H.tbody ! dataBind "foreach: scores" $
            H.tr $ do
              H.td ! dataBind "text: name" $ ""
              H.td ! dataBind "text: diff" $ ""
              H.td ! dataBind "text: formula" $ ""
      
dataBind :: AttributeValue -> Attribute
dataBind = dataAttribute "bind"
