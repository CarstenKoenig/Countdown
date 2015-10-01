{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Players
       ( isRegistered
       , registeredPlayer
       , registerPlayer
       )where

import Control.Monad.IO.Class(liftIO)

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe(isJust)

import Data.IORef (IORef(..), newIORef, atomicModifyIORef')

import Data.Text (Text, unpack)

import Web.Scotty
import qualified Web.Scotty as S

import Countdown.Game (Player(Player), nickName, playerId)

import CountdownGame.Database as Rep
import qualified CountdownGame.Cookies as Cookies
               
registeredPlayer :: ActionM (Maybe Player)
registeredPlayer = do
  cookie <- Cookies.getPlayerCookie
  case cookie of
    Nothing -> return Nothing
    Just c  -> liftIO $ Rep.checkPlayer (Cookies.playerId c) (Cookies.nickName c)

registerPlayer :: Text -> ActionM Player
registerPlayer nick = do
  cookie <- Cookies.getPlayerCookie
  player <- liftIO $ case cookie of
    Nothing -> Rep.addPlayer nick
    Just c  -> Rep.updatePlayer (Cookies.playerId c) nick
  let cookie = Cookies.PlayerCookie (nickName player) (playerId player)
  Cookies.setPlayerCookie cookie
  return player

isRegistered :: ActionM Bool
isRegistered = do
  player <- registeredPlayer
  return $ isJust player

fromCookie :: Cookies.PlayerCookie -> Player
fromCookie cookie = Player (Cookies.nickName cookie) (Cookies.playerId cookie)
