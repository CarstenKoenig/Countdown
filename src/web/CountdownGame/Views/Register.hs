{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Register where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Html
render = do
  html $ do
    body $ do
      h1 "COUNTdown"

      H.form H.! method "post" H.! action "/register" $ do
        H.input H.! type_ "text" H.! name "nickName"
        H.input H.! type_ "submit" H.! value "Anmelden"
