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
       , Snapshot
       , takeSnapshot
       , Reference
       , readRef
       , modifyRef
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import Data.Function (on)
import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Maybe (isJust, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (sortBy)

type PlayerId = Integer

data State =
  State
  { currentRound  :: Reference (Maybe Round)
  , nextRound     :: Reference (Maybe RoundParam)
  , players       :: Players
  , playerGuesses :: Reference (Map PlayerId Int)
  }

data Snapshot =
  Snapshot
  { goal         :: Maybe Int
  , availableNrs :: [Int]
  , isStartable  :: Bool
  , isRunning    :: Bool
  , secondsLeft  :: Maybe Int
  , scoreBoard   :: [(Text, Maybe Int)]
  } deriving (Generic, Show)

takeSnapshot :: State -> IO Snapshot
takeSnapshot state = do
  round <- readRef id $ currentRound state
  guesses <- readRef id $ playerGuesses state
  ps <- readRef id $ players state
  ready <- readRef isJust $ nextRound state
  now <- getCurrentTime
  let g = target . params <$> round
      nrs = maybe [] (numbers . params) round
      till = round >>= validTill
      secs = (`diffUTCTime` now) <$> till
      run = isJust round && fromMaybe (-1) secs > 0
      score = calculateScore g ps guesses
  return $ Snapshot g nrs (not run && ready) run (truncate <$> secs) score

calculateScore :: Maybe Int -> PlayersMap -> Map PlayerId Int -> [(Text, Maybe Int)]
calculateScore Nothing ps _ = map (\(_,nick) -> (nick,Nothing)) . M.toList $ M.map nickName ps
calculateScore (Just g) ps gm = sortBy (compare `on` snd) scores
  where
    scores = map assocGuess . M.toList $ M.map nickName ps
    assocGuess (pid, nick) = (nick, diff <$> M.lookup pid gm)
    diff pg = abs (g - pg)
  

instance ToJSON Snapshot
instance FromJSON Snapshot

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
  , validTill   :: Maybe UTCTime
  } deriving (Generic, Show)

instance ToJSON Round
instance FromJSON Round

data RoundParam =
  RoundParam
  { target  :: Int
  , numbers :: [Int]
  } deriving (Generic, Show)

instance ToJSON RoundParam
instance FromJSON RoundParam

newtype RoundParamState = RoundParamState (IORef (Maybe RoundParam))

initState :: IO State
initState = do
  players <- initializePlayers
  emptyR <- emptyRoundState
  emptyP <- emptyRoundParamState
  noGuesses <- Reference <$> newIORef M.empty
  return $ State emptyR emptyP players noGuesses

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
