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

import Countdown.Game.Players(Player (..), PlayerId, PlayersMap)
import qualified Countdown.Game.Players as P
import CountdownGame.References
import CountdownGame.State (Players)


getPlayer :: PlayerId -> Players -> IO (Maybe Player)
getPlayer id = readRef (P.lookup id)

getPlayers :: Players -> IO [Player]
getPlayers = readRef P.list

addPlayer :: Text -> Players -> IO Player
addPlayer nick = modifyRef (P.insert nick)

updatePlayer :: PlayerId -> Text -> Players -> IO Player
updatePlayer id nick = modifyRef (P.update id nick)
