{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Game
       ( PlayerId
       , Player (..)
       )where

import Data.Text.Lazy (Text)

type PlayerId = Integer

data Player =
  Player
  { nickName :: Text
  , playerId :: PlayerId
  } deriving (Show, Read)
