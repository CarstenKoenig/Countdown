{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.State
       ( State (..)
       , Round (..)
       , Players
       , initState
       , setAttempt
       , takeSnapshot
       )where

import Data.Text (Text)
import qualified Data.Map.Strict as M

import Countdown.Game.Attempts (Attempt, attempt)
import Countdown.Game.Challanges (Challange)
import Countdown.Game.Players (PlayerId)

import CountdownGame.References
import CountdownGame.State.Definitions (State (..), Round (..), Players)
import CountdownGame.State.Snapshots (takeSnapshot)

setAttempt :: State -> PlayerId -> Text -> IO (Maybe Attempt)
setAttempt state pid txt = do
  ch <- readRef (fmap challange) $ currentRound state
  case ch of
    Just ch' -> return <$> (modifyRef (attempt ch' txt pid) $ playerAttempts state)
    Nothing  -> return Nothing

initState :: IO State
initState = do
  ps <- initializePlayers
  emptyR <- emptyRoundState
  emptyP <- emptyChallange
  noGuesses <- createRef M.empty
  return $ State emptyR emptyP ps noGuesses

initializePlayers :: IO Players
initializePlayers = createRef M.empty

emptyRoundState :: IO (Reference (Maybe Round))
emptyRoundState = createRef Nothing

emptyChallange :: IO (Reference (Maybe Challange))
emptyChallange = createRef Nothing
