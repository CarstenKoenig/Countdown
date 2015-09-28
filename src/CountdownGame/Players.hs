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

import CountdownGame.Game
import CountdownGame.PlayersRepository (Players)
import CountdownGame.PlayersRepository as Rep
import qualified CountdownGame.Cookies as Cookies
               
registeredPlayer :: Players -> ActionM (Maybe Player)
registeredPlayer ps = do
  cookie <- Cookies.getPlayerCookie
  case cookie of
    Nothing -> return Nothing
    Just c  -> liftIO $ Rep.getPlayer (Cookies.playerId c) ps

registerPlayer :: Text -> Players -> ActionM Player
registerPlayer nick ps = do
  cookie <- Cookies.getPlayerCookie
  player <- liftIO $ case cookie of
    Nothing -> Rep.addPlayer nick ps
    Just c  -> Rep.updatePlayer (Cookies.playerId c) nick ps
  let cookie = Cookies.PlayerCookie (nickName player) (playerId player)
  Cookies.setPlayerCookie cookie
  return player

isRegistered :: Players -> ActionM Bool
isRegistered ps = do
  player <- registeredPlayer ps
  return $ isJust player

fromCookie :: Cookies.PlayerCookie -> Player
fromCookie cookie = Player (Cookies.nickName cookie) (Cookies.playerId cookie)
