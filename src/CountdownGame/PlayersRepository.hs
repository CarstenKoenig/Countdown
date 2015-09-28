{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.PlayersRepository
       ( Players
       , initializePlayers
       , getPlayer
       , getPlayers
       , addPlayer
       , updatePlayer
       )where

import Data.Text.Lazy (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import CountdownGame.Game

type PlayersMap = Map PlayerId Player
  
newtype Players = Players (IORef (PlayersMap))

initializePlayers :: IO Players
initializePlayers = Players <$> newIORef M.empty

getPlayer :: PlayerId -> Players -> IO (Maybe Player)
getPlayer id = readRef (getPlayer' id)

getPlayers :: Players -> IO [Player]
getPlayers = readRef getPlayers'

addPlayer :: Text -> Players -> IO Player
addPlayer nick = useRef (addPlayer' nick)

updatePlayer :: PlayerId -> Text -> Players -> IO Player
updatePlayer id nick = useRef (updatePlayer' id nick)

readRef :: (PlayersMap -> a) -> Players -> IO a
readRef f (Players ref) = f <$> readIORef ref

useRef :: (PlayersMap -> (PlayersMap, a)) -> Players -> IO a
useRef f (Players ref) = atomicModifyIORef' ref f

getPlayers' :: PlayersMap -> [Player]
getPlayers' = map snd . M.toAscList

getPlayer' :: PlayerId -> PlayersMap -> Maybe Player
getPlayer' id = M.lookup id

addPlayer' :: Text -> PlayersMap -> (PlayersMap, Player)
addPlayer' nick m =
  let id = fromIntegral $ M.size m
  in updatePlayer' id nick m

updatePlayer' :: PlayerId -> Text -> PlayersMap -> (PlayersMap, Player)
updatePlayer' id nick m =
  let p = Player nick id
  in (M.alter (const $ Just p) id m, p)
  
