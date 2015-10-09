{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.State
       ( State (..)
       , initState
       , versuchHinzufuegen
       , takeSnapshot
       )where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as M

import Countdown.Game (Attempt, Challange, Player, PlayerId, playerId, score, attempt)

import CountdownGame.References
import CountdownGame.State.Definitions (State (..), SpielParameter (..), Phasen (..))
import qualified CountdownGame.State.Definitions as Def
import CountdownGame.State.Snapshots (takeSnapshot)
import CountdownGame.State.Rounds (startGameLoop)
import CountdownGame.Database (setPlayerScore, createPool)

versuchHinzufuegen :: State -> Player -> Text -> IO (Maybe Attempt)
versuchHinzufuegen state p f = do
  phase <- readRef id $ aktuellePhase state
  case phase of
    (RundePhase _ chal vers key _) -> do
      v <- modifyRef (attempt chal f p) vers
      setPlayerScore key (playerId p) (score v)
      return $ Just v
    _ -> return Nothing


initState :: Int -> IO State
initState nrPoolCons = do
  phases <- startGameLoop (SpielParameter 60 30)
  pool <- createPool nrPoolCons
  return $ State phases pool
