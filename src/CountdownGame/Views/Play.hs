{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Play where

import Data.Text (append)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import CountdownGame.Game

render :: Player -> Html
render player = html $ do
  script ! A.src "jquery.js" $ text ""
  script ! A.src "knockout.js" $ text ""
  script ! A.src "player.js" $ text ""
  body $
    H.div ! A.class_ "Main" $ do
      h1 "COUNTdown"
      h3 . text $  "Hallo " `append` nickName player
  
      H.div ! A.class_ "Guess" $ do
        p $ do
          H.span "Ziel: "
          H.span ! dataBind "text: goal" $ ""
        p $ do
          H.span "Zahlen: "
          H.span ! dataBind "text: numbers" $ ""
        p $ do
          H.span "Dein Versuch: "
          H.span ! A.class_ "Result" ! dataBind "text: result" $ ""
        H.div ! A.class_ "Countdown" ! dataBind "visible: isRunning" $ do
          p ! dataBind "text: secondsLeft" $ ""
        H.div ! A.class_ "FormulaInput" ! dataBind "visible: isRunning" $ do
          p "Deine Formel:"
          H.form $ do
            H.input ! A.type_ "text" ! A.name "formula" ! A.autofocus "" ! dataBind "value: formula"
            H.input ! type_ "submit" ! dataBind "click: eval" ! value "OK"
          p ! A.class_ "Error" ! dataBind "text: error" $ ""
      
dataBind :: AttributeValue -> Attribute
dataBind = dataAttribute "bind"

