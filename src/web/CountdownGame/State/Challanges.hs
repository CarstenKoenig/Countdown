{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.State.Challanges
       ( isNextReady
       , getNext
       , startGeneration
       )where

import Control.Concurrent (forkIO, threadDelay, ThreadId)

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (isJust)
import Data.Time (getCurrentTime, addUTCTime)

import Countdown.Game (Challange(..), generateChallange)
import CountdownGame.References
import CountdownGame.State.Definitions (State, nextChallange)


isNextReady :: State -> IO Bool
isNextReady state = isJust <$> getNext state

getNext :: State -> IO (Maybe Challange)
getNext = readRef id . nextChallange

startGeneration :: State -> IO ()
startGeneration state = do
  clearRef (nextChallange state)
  _ <- forkIO work
  return ()
  where
    work = do
      newRound <- generateChallange
      modifyRef (const (Just newRound, ())) (nextChallange state)
