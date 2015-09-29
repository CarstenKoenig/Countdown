{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Rounds
       ( getRound
       , startGenerateRound
       , startRound
       )where

import Debug.Trace (trace)

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import Data.Time (getCurrentTime, addUTCTime)

import CountdownGame.Game

startRound :: State -> IO ()
startRound state = do
  next <- getRoundParam (nextRound state)
  time <- (30 `addUTCTime`) <$> getCurrentTime
  case next of
    Just n -> do
      let r = Round n [] (Just time)
      modifyRef (const (Just r, ())) (currentRound state)
      startGenerateRound (nextRound state)
    Nothing -> return ()

getRound :: Reference (Maybe Round) -> IO (Maybe Round)
getRound = readRef id

getRoundParam :: Reference (Maybe RoundParam) -> IO (Maybe RoundParam)
getRoundParam = readRef id

startGenerateRound :: Reference (Maybe RoundParam) -> IO ()
startGenerateRound rs = do
  clearRef rs
  _ <- forkIO work
  return ()
  where
    work = do
      newRound <- trace "erzeuge Runde..." gen
      modifyRef (const (Just newRound, trace "...erzeugt" ())) rs
    gen :: IO RoundParam
    gen = do
      threadDelay 10000000
      return $ RoundParam 765 [1,3,7,10,25,50]

clearRef :: Reference (Maybe a) -> IO ()
clearRef = modifyRef (const (Nothing, ()))
