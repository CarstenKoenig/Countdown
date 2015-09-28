{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CountdownGame.Game
       ( PlayerId
       , Player (..)
       , Players (..)
       , PlayersMap
       , Round (..)
       , RoundState (..)
       , State (..)
       , initState
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import Data.Text (Text)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.IORef (IORef(..), newIORef)

type PlayerId = Integer

data State =
  State
  { currentRound :: RoundState
  , players      :: Players
  }

data Player =
  Player
  { nickName :: Text
  , playerId :: PlayerId
  } deriving (Generic, Show)

instance ToJSON Player
instance FromJSON Player

newtype Players = Players (IORef (PlayersMap))
type PlayersMap = Map PlayerId Player
         
data Round =
  Round
  { target  :: Integer
  , numbers :: [Integer]
  } deriving (Generic, Show)

instance ToJSON Round
instance FromJSON Round

newtype RoundState = RoundState (IORef (Maybe Round))

initState :: IO State
initState = do
  players <- initializePlayers
  currentRound <- emptyRoundState
  return $ State currentRound players

initializePlayers :: IO Players
initializePlayers = Players <$> newIORef M.empty

emptyRoundState :: IO RoundState
emptyRoundState = RoundState <$> newIORef Nothing
