{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.State
       ( State (..)
       , Round (..)
       , initState
       , setAttempt
       , takeSnapshot
       )where

import Data.Text (Text)
import qualified Data.Map.Strict as M

import Countdown.Game (Attempt, attempt, Challange, PlayerId, score)

import CountdownGame.References
import CountdownGame.State.Definitions (State (..), Round (..))
import CountdownGame.State.Snapshots (takeSnapshot)
import CountdownGame.Database (setPlayerScore)

setAttempt :: State -> PlayerId -> Text -> IO (Maybe Attempt)
setAttempt state pid txt = do
  rd <- readRef id $ currentRound state
  case rd of
    Just rd' -> do
      let ch  = challange rd'
          cId = databaseKey rd'
      at <- modifyRef (attempt ch txt pid) $ playerAttempts state
      setPlayerScore cId pid (score at)
      return $ Just at
    Nothing  -> return Nothing

initState :: IO State
initState = do
  emptyR <- emptyRoundState
  emptyP <- emptyChallange
  noGuesses <- createRef M.empty
  return $ State emptyR emptyP noGuesses

emptyRoundState :: IO (Reference (Maybe Round))
emptyRoundState = createRef Nothing

emptyChallange :: IO (Reference (Maybe Challange))
emptyChallange = createRef Nothing
