{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.PlayersRepository
       ( getPlayer
       , getPlayers
       , addPlayer
       , updatePlayer
       )where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import CountdownGame.Game

getPlayer :: PlayerId -> Players -> IO (Maybe Player)
getPlayer id = readRef (getPlayer' id)

getPlayers :: Players -> IO [Player]
getPlayers = readRef getPlayers'

addPlayer :: Text -> Players -> IO Player
addPlayer nick = modifyRef (addPlayer' nick)

updatePlayer :: PlayerId -> Text -> Players -> IO Player
updatePlayer id nick = modifyRef (updatePlayer' id nick)

getPlayers' :: PlayersMap -> [Player]
getPlayers' = map snd . M.toAscList

getPlayer' :: PlayerId -> PlayersMap -> Maybe Player
getPlayer' = M.lookup

addPlayer' :: Text -> PlayersMap -> (PlayersMap, Player)
addPlayer' nick m =
  let id = fromIntegral $ M.size m
  in updatePlayer' id nick m

updatePlayer' :: PlayerId -> Text -> PlayersMap -> (PlayersMap, Player)
updatePlayer' id nick m =
  let p = Player nick id
  in (M.alter (const $ Just p) id m, p)
  
