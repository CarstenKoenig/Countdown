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
      modRef (const (Just r, ())) (currentRound state)
      startGenerateRound (nextRound state)
    Nothing -> return ()

getRound :: RoundState -> IO (Maybe Round)
getRound = readRef id

getRoundParam :: RoundParamState -> IO (Maybe RoundParam)
getRoundParam = readParamRef id

startGenerateRound :: RoundParamState -> IO ()
startGenerateRound rs = do
  clearParamRef rs
  _ <- forkIO work
  return ()
  where
    work = do
      newRound <- trace "erzeuge Runde..." gen
      modParamRef (const (Just newRound, trace "...erzeugt" ())) rs
    gen :: IO RoundParam
    gen = do
      threadDelay 10000000
      return $ RoundParam 765 [1,3,7,10,25,50]

clearRef :: RoundState -> IO ()
clearRef = modRef (const (Nothing, ()))
  
readRef :: (Maybe Round -> a) -> RoundState -> IO a
readRef f (RoundState ref) = f <$> readIORef ref

modRef :: (Maybe Round -> (Maybe Round, a)) -> RoundState -> IO a
modRef f (RoundState ref) = atomicModifyIORef' ref f

clearParamRef :: RoundParamState -> IO ()
clearParamRef = modParamRef (const (Nothing, ()))

readParamRef :: (Maybe RoundParam -> a) -> RoundParamState -> IO a
readParamRef f (RoundParamState ref) = f <$> readIORef ref

modParamRef :: (Maybe RoundParam -> (Maybe RoundParam, a)) -> RoundParamState -> IO a
modParamRef f (RoundParamState ref) = atomicModifyIORef' ref f
