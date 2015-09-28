{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Rounds
       ( getRound
       )where

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import CountdownGame.Game

getRound :: RoundState -> IO (Maybe Round)
getRound = readRef id

readRef :: (Maybe Round -> a) -> RoundState -> IO a
readRef f (RoundState ref) = f <$> readIORef ref
