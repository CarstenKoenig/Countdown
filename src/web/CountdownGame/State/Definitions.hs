{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CountdownGame.State.Definitions
       ( Players
       , Round (..)
       , State (..)
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Countdown.Game (Attempt, AttemptsMap, Challange, Player, PlayersMap)

import CountdownGame.References

data State =
  State
  { currentRound   :: Reference (Maybe Round)
  , nextChallange  :: Reference (Maybe Challange)
  , players        :: Players
  , playerAttempts :: Attempts
  }

type Players = Reference PlayersMap

data Round =
  Round
  { challange   :: Challange
  , validTill   :: Maybe UTCTime
  } deriving (Generic, Show)

instance ToJSON Round

type Attempts = Reference AttemptsMap
