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

import CountdownGame.PlayersRepository (Players)
import qualified CountdownGame.Views.Play as PlayView
import qualified CountdownGame.Views.Register as RegisterView
import qualified CountdownGame.Views.Admin as AdminView
import qualified CountdownGame.Players as Players

play :: Players -> ActionM ()
play ps = do
    registered <- Players.isRegistered ps
    if not registered
      then redirect "/register"
      else render PlayView.render
              
register :: ActionM ()
register = render RegisterView.render

postRegister :: Players -> ActionM ()
postRegister ps = do
  name <- param "nickName"
  Players.registerPlayer name ps
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
