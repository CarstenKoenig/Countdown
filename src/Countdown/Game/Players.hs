{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Countdown.Game.Players
       ( PlayerId
       , Player (..)
       , PlayersMap
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Map.Strict (Map)

type PlayerId = Integer

data Player =
  Player
  { nickName :: Text
  , playerId :: PlayerId
  } deriving (Generic, Show)

instance ToJSON Player

type PlayersMap = Map PlayerId Player
