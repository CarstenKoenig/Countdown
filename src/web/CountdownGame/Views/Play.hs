{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Play where

import Data.Text (append)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Countdown.Game (Player, nickName)

render :: Player -> Html
render player = html $ do
  script ! A.src "jquery.js" $ text ""
  script ! A.src "jquery.timer.js" $ text ""
  script ! A.src "knockout.js" $ text ""
  script ! A.src "player.js" $ text ""
  body $ do
    H.div ! A.class_ "overlay" ! dataBind "visible: gotBusted" $ do
      H.div ! A.class_ "centerMessage" $ do
        h1 "damn it.."
        H.div ! A.class_ "Error" ! dataBind "html: error" $ ""
      
    H.div ! A.class_ "Main" $ do
      h1 "COUNTdown"
      h3 . text $  "Hallo " `append` nickName player

      H.div ! A.class_ "Countdown" $ do
        p ! dataBind "text: secondsLeft" $ ""

      H.div ! A.class_ "Aufgabe" ! dataBind "visible: isRunning" $ do
        p $ do
          H.span "Ziel: "
          H.span ! dataBind "text: goal" $ ""
        p $ do
          H.span "Zahlen: "
          H.span ! dataBind "text: numbers" $ ""
  
      H.div ! A.class_ "Guess" ! dataBind "visible: isRunning" $ do
        p $ do
          H.span "letztes Ergebnis: "
          H.span ! A.class_ "Result" ! dataBind "text: result" $ ""
        H.div ! A.class_ "FormulaInput" ! dataBind "visible: isRunning" $ do
          p "dein Loesungsvorschlag: "
          H.form $ do
            H.input ! A.type_ "text" ! A.name "formula" ! A.autofocus "" ! dataBind "value: formula"
            H.input ! type_ "submit" ! dataBind "click: eval" ! value "OK"
            
      H.div ! A.class_ "Ergebnisse" ! dataBind "visible: isWaiting" $ do
        H.table $ do
          H.thead $ do
            H.td "Spieler"
            H.td "Punkte"
          H.tbody ! dataBind "foreach: scores" $
            H.tr $ do
              H.td ! dataBind "text: name" $ ""
              H.td ! dataBind "text: score" $ ""

      H.div ! A.class_ "Error" ! dataBind "html: error" $ ""
         
      
dataBind :: AttributeValue -> Attribute
dataBind = dataAttribute "bind"

