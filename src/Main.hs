{-# LANGUAGE OverloadedStrings #-}

import Data.Char (toLower)
import Data.List(isPrefixOf)

import Web.Scotty

import Web.Scotty
import qualified Web.Scotty as S

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (link, (!))
import Text.Blaze.Html5.Attributes

import Network.Socket (SockAddr(..))
import Network.Wai (remoteHost)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)

import qualified CountdownGame.Views.Index
import qualified CountdownGame.Views.Play
import qualified CountdownGame.Views.Admin

main :: IO ()
main = scotty 8080 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")
  get "/" $ do
    localHost <- isLocalhost
    if localHost
      then redirect "/admin"
      else redirect "/play"
  get "/play" $ do
    blaze $ link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
    blaze CountdownGame.Views.Play.render
  get "/admin" $ do
    localhost <- isLocalhost
    if not localhost
      then redirect "/admin"
      else do
         blaze $ link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
         blaze CountdownGame.Views.Admin.render

isLocalhost :: ActionM Bool
isLocalhost = do
  remote <- remoteHost <$> request
  return $ hostnameIsLocal remote
  where hostnameIsLocal sockAdr =
          "127.0.0.1" `isPrefixOf` show sockAdr ||
          "localhost" `isPrefixOf` (map toLower . show) sockAdr

blaze :: Html -> ActionM ()
blaze = S.html . renderHtml
