{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CountdownGame.State.Definitions
       ( Round (..)
       , State (..)
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Countdown.Game (Attempt, AttemptsMap, Challange, Player, PlayersMap)

import CountdownGame.References

data State =
  State
  { currentRound   :: Reference (Maybe Round)
  , nextChallange  :: Reference (Maybe Challange)
  , playerAttempts :: Attempts
  }

data Round =
  Round
  { challange   :: Challange
  , databaseKey :: Int64
  , validTill   :: Maybe UTCTime
  } deriving (Generic, Show)

instance ToJSON Round

type Attempts = Reference AttemptsMap
