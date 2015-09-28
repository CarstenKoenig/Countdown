{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Actions
       ( play
       , register
       , postRegister
       , admin
       , isLocalhost
       )where

import Debug.Trace (trace)

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Maybe(isJust)

import Data.Text.Lazy (Text, unpack)

import Web.Scotty
import qualified Web.Scotty as S

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Text.Blaze.Html5 (link, (!))
import Text.Blaze.Html5.Attributes

import Network.Socket (SockAddr(..))
import Network.Wai (remoteHost)

import qualified CountdownGame.Views.Play as PlayView
import qualified CountdownGame.Views.Register as RegisterView
import qualified CountdownGame.Views.Admin as AdminView
import qualified CountdownGame.Cookies as Cookies

play :: ActionM ()
play = do
    registered <- isRegistered
    if not registered
      then redirect "/register"
      else render PlayView.render
              
register :: ActionM ()
register = render RegisterView.render

postRegister :: ActionM ()
postRegister = do
  name <- param "nickName"
  let cookie = Cookies.PlayerCookie name 1
  Cookies.setPlayerCookie cookie
  redirect "/play"         

admin :: ActionM ()
admin = do
    localhost <- isLocalhost
    if not localhost
      then redirect "/admin"
      else render AdminView.render

render :: Html -> ActionM ()
render html = do
  blaze $ link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
  blaze html
  
blaze :: Html -> ActionM ()
blaze = S.html . renderHtml

isLocalhost :: ActionM Bool
isLocalhost = do
  remote <- remoteHost <$> request
  return $ hostnameIsLocal remote
  where hostnameIsLocal sockAdr =
          "127.0.0.1" `isPrefixOf` show sockAdr ||
          "localhost" `isPrefixOf` (map toLower . show) sockAdr

isRegistered :: ActionM Bool
isRegistered = do
  player <- Cookies.getPlayerCookie
  trace (maybe "Cookie not set!" (unpack . Cookies.nickName) player) $ return $ isJust player
