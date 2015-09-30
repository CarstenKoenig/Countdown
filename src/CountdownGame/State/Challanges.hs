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

import Countdown.Game.Challanges (Challange(..))
import CountdownGame.References
import CountdownGame.State

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
      newRound <- gen
      modifyRef (const (Just newRound, ())) (nextChallange state)
    gen :: IO Challange
    gen = do
      threadDelay 10000000
      return $ Challange 765 [1,3,7,10,25,50]
