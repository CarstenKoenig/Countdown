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

startNext :: State -> IO ()
startNext state = do
  next <- getNext state
  time <- (30 `addUTCTime`) <$> getCurrentTime
  case next of
    Just n -> do
      let r = Round n (Just time)
      modifyRef (const (Just r, ())) (currentRound state)
      startGeneration state
    Nothing -> return ()

current :: State -> IO (Maybe Round)
current = readRef id . currentRound
