{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Players
       ( isRegistered
       , registeredPlayer
       , registerPlayer
       )where

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Maybe(isJust)

import Data.Text.Lazy (Text, unpack)

import Web.Scotty
import qualified Web.Scotty as S

import qualified CountdownGame.Cookies as Cookies

type PlayerId = Integer

data Player =
  Player
  { nickName :: Text
  , playerId :: PlayerId
  } deriving (Show, Read)
                

registeredPlayer :: ActionM (Maybe Player)
registeredPlayer = do
  cookie <- Cookies.getPlayerCookie
  return $ fromCookie <$> cookie

registerPlayer :: PlayerId -> Text -> ActionM Player
registerPlayer id nick = do
  let cookie = Cookies.PlayerCookie nick id
  Cookies.setPlayerCookie cookie
  return $ fromCookie cookie

isRegistered :: ActionM Bool
isRegistered = do
  player <- registeredPlayer
  return $ isJust player

fromCookie :: Cookies.PlayerCookie -> Player
fromCookie cookie = Player (Cookies.nickName cookie) (Cookies.playerId cookie)
