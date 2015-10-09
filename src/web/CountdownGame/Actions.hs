{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Actions
       ( play
       , register
       , postRegister
       , admin
       , getPlayers
       , getSnapshot
       , startRound
       , evalFormula
       , isLocalhost
       , initCompletion
       , nextCompletion
       )where

import Debug.Trace (trace)

import Control.Monad.IO.Class(liftIO)

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Maybe(isNothing, fromJust, isJust)

import Data.Text.Lazy (Text, unpack)
import qualified Data.Text as T

import Web.Scotty
import qualified Web.Scotty as S

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Text.Blaze.Html5 (link, (!))
import Text.Blaze.Html5.Attributes

import Network.Socket (SockAddr(..))
import Network.Wai (remoteHost)

import Countdown.Game (PlayerId)
import qualified Countdown.Game as G

import CountdownGame.State (State (..), takeSnapshot, setAttempt, initialCompletions, completions)
import CountdownGame.Players (registeredPlayer)
import qualified CountdownGame.Database as Rep
import qualified CountdownGame.State.Rounds as Rounds

import qualified CountdownGame.Views.Play as PlayView
import qualified CountdownGame.Views.Register as RegisterView
import qualified CountdownGame.Views.Admin as AdminView
import qualified CountdownGame.Players as Players

-- * controller actions

play :: State -> ActionM ()
play state = do
    player <- Players.registeredPlayer state
    if isNothing player
      then redirect "/register"
      else render $ PlayView.render (fromJust player)
              
register :: ActionM ()
register = render RegisterView.render

postRegister :: State -> ActionM ()
postRegister state = do
  name <- param "nickName"
  Players.registerPlayer name state
  redirect "/play"         

admin :: State -> ActionM ()
admin state = do
  localhost <- isLocalhost
  if not localhost
    then raise "you are not allowed"
    else render (AdminView.render state)

-- * Web-API Part

getPlayers :: State -> ActionM ()
getPlayers state = do
  players <- liftIO $ Rep.getPlayers (connectionPool state)
  localhost <- isLocalhost
  if not localhost
    then raise "you are not allowed to do that"
    else json players

getSnapshot :: State -> ActionM ()
getSnapshot state = do
  isHost <- isLocalhost
  regPlayer <- isJust <$> registeredPlayer state
  if not isHost && not regPlayer
    then raise "you are not allowed to do that"
    else do
      snap <- liftIO $ takeSnapshot isHost state
      json snap

startRound :: State -> ActionM ()
startRound state = do
  localhost <- isLocalhost
  if not localhost
    then raise "you are not allowed to do that"
    else do
      liftIO $ Rounds.startNext state
      getSnapshot state
  

evalFormula :: State -> ActionM ()
evalFormula state = do
  formula <- param "formula"
  pl <- registeredPlayer state
  case pl of
    Just pl' -> do
      att <- liftIO $ setAttempt state pl' formula
      json att
    Nothing -> raise "kein Spieler registriert"

initCompletion :: State -> ActionM ()
initCompletion state = do
  cps <- liftIO $ initialCompletions state
  json cps
    
nextCompletion :: State -> ActionM ()
nextCompletion state = do
  inp <- jsonData
  cps <- liftIO $ completions state inp
  json cps

-- * Helpers

-- ** rendering
  
render :: Html -> ActionM ()
render html = do
  blaze $ do
    link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
    html
  
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
