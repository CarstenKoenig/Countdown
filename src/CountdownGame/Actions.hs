{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Actions
       ( play
       , register
       , postRegister
       , admin
       , getPlayers
       , getRound
       , isLocalhost
       )where

import Control.Monad.IO.Class(liftIO)

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Maybe(isNothing, fromJust)

import Data.Text.Lazy (Text, unpack)

import Web.Scotty
import qualified Web.Scotty as S

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Text.Blaze.Html5 (link, (!))
import Text.Blaze.Html5.Attributes

import Network.Socket (SockAddr(..))
import Network.Wai (remoteHost)

import CountdownGame.Game

import qualified CountdownGame.PlayersRepository as Rep
import qualified CountdownGame.Rounds as Rounds

import qualified CountdownGame.Views.Play as PlayView
import qualified CountdownGame.Views.Register as RegisterView
import qualified CountdownGame.Views.Admin as AdminView
import qualified CountdownGame.Players as Players

-- * controller actions

play :: State -> ActionM ()
play state = do
    player <- Players.registeredPlayer (players state)
    if isNothing player
      then redirect "/register"
      else render $ PlayView.render (fromJust player)
              
register :: ActionM ()
register = render RegisterView.render

postRegister :: State -> ActionM ()
postRegister state = do
  name <- param "nickName"
  Players.registerPlayer name (players state)
  redirect "/play"         

admin :: State -> ActionM ()
admin state = do
  players <- liftIO $ Rep.getPlayers (players state)
  nextRound <- liftIO $ Rounds.getRound (nextRound state)
  localhost <- isLocalhost
  if not localhost
    then redirect "/admin"
    else render (AdminView.render players nextRound)

-- * Web-API Part

getPlayers :: State -> ActionM ()
getPlayers state = do
  players <- liftIO $ Rep.getPlayers (players state)
  localhost <- isLocalhost
  if not localhost
    then raise "you are not allowed to do that"
    else json players

getRound :: State -> ActionM ()
getRound state = do
  round <- liftIO $ Rounds.getRound (currentRound state)
  json round

-- * Helpers

-- ** rendering
  
render :: Html -> ActionM ()
render html = do
  blaze $ link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
  blaze html
  
blaze :: Html -> ActionM ()
blaze = S.html . renderHtml

-- ** access checks

isLocalhost :: ActionM Bool
isLocalhost = do
  remote <- remoteHost <$> request
  return $ hostnameIsLocal remote
  where hostnameIsLocal sockAdr =
          "127.0.0.1" `isPrefixOf` show sockAdr ||
          "localhost" `isPrefixOf` (map toLower . show) sockAdr
