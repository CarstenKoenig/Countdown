{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Rounds
       ( 
       )where

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import CountdownGame.Game
