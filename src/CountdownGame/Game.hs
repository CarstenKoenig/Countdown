{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CountdownGame.Game
       ( PlayerId
       , Player (..)
       , Players (..)
       , PlayersMap
       , Round (..)
       , RoundParam(..)
       , State (..)
       , initState
       , Reference
       , readRef
       , modifyRef
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

type PlayerId = Integer

data State =
  State
  { currentRound :: Reference (Maybe Round)
  , nextRound    :: Reference (Maybe RoundParam)
  , players      :: Players
  }

data Player =
  Player
  { nickName :: Text
  , playerId :: PlayerId
  } deriving (Generic, Show)

instance ToJSON Player
instance FromJSON Player

type Players = Reference PlayersMap
type PlayersMap = Map PlayerId Player
         
data Round =
  Round
  { params      :: RoundParam
  , guesses     :: [(PlayerId,Integer)]
  , runningTill :: Maybe UTCTime
  } deriving (Generic, Show)

instance ToJSON Round
instance FromJSON Round

data RoundParam =
  RoundParam
  { target  :: Integer
  , numbers :: [Integer]
  } deriving (Generic, Show)

instance ToJSON RoundParam
instance FromJSON RoundParam

newtype RoundParamState = RoundParamState (IORef (Maybe RoundParam))

initState :: IO State
initState = do
  players <- initializePlayers
  emptyR <- emptyRoundState
  emptyP <- emptyRoundParamState
  return $ State emptyR emptyP players

initializePlayers :: IO Players
initializePlayers = Reference <$> newIORef M.empty

emptyRoundState :: IO (Reference (Maybe Round))
emptyRoundState = Reference <$> newIORef Nothing

emptyRoundParamState :: IO (Reference (Maybe RoundParam))
emptyRoundParamState = Reference <$> newIORef Nothing

newtype Reference a = Reference { refOf :: IORef a }

readRef :: (a -> b) -> Reference a -> IO b
readRef f ref = f <$> readIORef (refOf ref)

modifyRef :: (a -> (a, b)) -> Reference a -> IO b
modifyRef f ref = atomicModifyIORef' (refOf ref) f
