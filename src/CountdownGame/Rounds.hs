{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Rounds
       ( getRound
       , nextRoundParams
       , startRound
       , startGenerateRound
       )where

import Debug.Trace (trace)

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import Data.Time (getCurrentTime, addUTCTime)

import CountdownGame.Game

startRound :: State -> IO ()
startRound state = do
  next <- nextRoundParams state
  time <- (30 `addUTCTime`) <$> getCurrentTime
  case next of
    Just n -> do
      let r = Round n (Just time)
      modifyRef (const (Just r, ())) (currentRound state)
      startGenerateRound state
    Nothing -> return ()

getRound :: State -> IO (Maybe Round)
getRound = readRef id . currentRound

nextRoundParams :: State -> IO (Maybe RoundParam)
nextRoundParams = readRef id . nextRound

startGenerateRound :: State -> IO ()
startGenerateRound state = do
  clearRef (nextRound state)
  _ <- forkIO work
  return ()
  where
    work = do
      newRound <- trace "erzeuge Runde..." gen
      modifyRef (const (Just newRound, trace "...erzeugt" ())) (nextRound state)
    gen :: IO RoundParam
    gen = do
      threadDelay 10000000
      return $ RoundParam 765 [1,3,7,10,25,50]

clearRef :: Reference (Maybe a) -> IO ()
clearRef = modifyRef (const (Nothing, ()))
