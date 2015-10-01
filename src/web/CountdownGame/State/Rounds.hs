{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.State.Rounds
       ( current
       , startNext
       )where

import Debug.Trace (trace)

import Control.Concurrent (forkIO, threadDelay, ThreadId)

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (isJust)
import Data.Time (getCurrentTime, addUTCTime)

import CountdownGame.State.Challanges
import CountdownGame.References
import CountdownGame.State.Definitions (State (currentRound), Round (Round))
import CountdownGame.Database (insertChallange)

startNext :: State -> IO ()
startNext state = do
  next <- getNext state
  time <- (30 `addUTCTime`) <$> getCurrentTime
  case next of
    Just n -> do
      chId <- insertChallange n
      let r = Round n chId (Just time)
      modifyRef (const (Just r, ())) (currentRound state)
      startGeneration state
    Nothing -> return ()

current :: State -> IO (Maybe Round)
current = readRef id . currentRound
