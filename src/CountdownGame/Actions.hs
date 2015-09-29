{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Actions
       ( play
       , register
       , postRegister
       , admin
       , getPlayers
       , getRound
       , startRound
       , getScores
       , evalFormula
       , isLocalhost
       )where

import Debug.Trace (trace)

import Control.Monad.IO.Class(liftIO)

import Data.Char (toLower)
import Data.Function (on)
import Data.List(isPrefixOf, sortBy)
import Data.Maybe(isNothing, fromJust)

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

import CountdownGame.Game
import qualified CountdownGame.Game as G

import qualified CountdownGame.PlayersRepository as Rep
import qualified CountdownGame.Rounds as Rounds
import CountdownGame.Players (registeredPlayer)

import CountdownGame.Algorithm (eval)
import CountdownGame.Parser (tryParse)

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
  curRound <- liftIO $ Rounds.getRound state
  localhost <- isLocalhost
  if not localhost
    then redirect "/admin"
    else render (AdminView.render players curRound)

-- * Web-API Part

getPlayers :: State -> ActionM ()
getPlayers state = do
  players <- liftIO $ Rep.getPlayers (players state)
  localhost <- isLocalhost
  if not localhost
    then raise "you are not allowed to do that"
    else json players

getScores :: State -> ActionM ()
getScores state = do
  round <- liftIO $ Rounds.getRound state
  ps <- liftIO $ Rep.getPlayers (players state)
  gs <- liftIO $ readRef (\x->x) (guesses state)
  let goal = maybe (-1) (G.target . G.params) $ round
  json (scores goal ps gs)

getRound :: State -> ActionM ()
getRound state = do
  round <- liftIO $ Rounds.getRound state
  json round

startRound :: State -> ActionM ()
startRound state = do
  localhost <- isLocalhost
  if not localhost
    then raise "you are not allowed to do that"
    else do
      liftIO $ Rounds.startRound state
      getRound state
  

evalFormula :: State -> ActionM ()
evalFormula state = do
  formula <- param "formula"
  player <- registeredPlayer (players state)
  case (eval <$> tryParse formula) of
    Just [n]  -> trace (show player) $ json n
    otherwise -> raise "invalid formula"

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

scores :: Integer -> [Player] -> [(PlayerId, Integer)] -> [(T.Text, Integer)]
scores goal ps gs =
  sortBy (compare `on` snd) $ map score ps
  where score p = case lookup (playerId p) gs of
          Nothing -> (nickName p, 999999)
          Just v  -> (nickName p, abs (goal - v))
