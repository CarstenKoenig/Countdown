{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Rounds
       ( getRound
       , startGenerateRound
       , startRound
       )where

import Debug.Trace (trace)

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import CountdownGame.Game

startRound :: State -> IO ()
startRound state = do
  next <- getRound (nextRound state)
  case next of
    Just n -> do
      modRef (const (Just n, ())) (currentRound state)
      startGenerateRound (nextRound state)
    Nothing -> return ()

getRound :: RoundState -> IO (Maybe Round)
getRound = readRef id

startGenerateRound :: RoundState -> IO ()
startGenerateRound rs = do
  clearRef rs
  _ <- forkIO work
  return ()
  where
    work = do
      newRound <- trace "erzeuge Runde..." gen
      modRef (const (Just newRound, trace "...erzeugt" ())) rs
    gen :: IO Round
    gen = do
      threadDelay 10000000
      return $ Round 765 [1,3,7,10,25,50]

clearRef :: RoundState -> IO ()
clearRef = modRef (const (Nothing, ()))
  
readRef :: (Maybe Round -> a) -> RoundState -> IO a
readRef f (RoundState ref) = f <$> readIORef ref

modRef :: (Maybe Round -> (Maybe Round, a)) -> RoundState -> IO a
modRef f (RoundState ref) = atomicModifyIORef' ref f
