{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Game
       ( PlayerId
       , Player (..)
       , Round (..)
       )where

import Data.Text (Text)

type PlayerId = Integer

data Player =
  Player
  { nickName :: Text
  , playerId :: PlayerId
  } deriving (Show, Read)

data Round =
  Round
  { target  :: Integer
  , numbers :: [Integer]
  } deriving (Show, Read)    
