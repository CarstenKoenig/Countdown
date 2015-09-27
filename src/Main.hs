{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Web.Scotty
import qualified Web.Scotty as S

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (link, (!))
import Text.Blaze.Html5.Attributes

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)

import qualified CountdownGame.Views.Index

main :: IO ()
main = scotty 8080 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")
  get "/" $ do
    blaze $ link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
    blaze CountdownGame.Views.Index.render

blaze :: Html -> ActionM ()
blaze = S.html . renderHtml
