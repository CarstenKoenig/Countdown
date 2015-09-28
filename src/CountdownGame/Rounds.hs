{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Rounds
       ( RoundState
       , emptyRoundState
       )where

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import CountdownGame.Game

newtype RoundState = RoundState (IORef (Maybe Round))

emptyRoundState :: IO RoundState
emptyRoundState = RoundState <$> newIORef Nothing
