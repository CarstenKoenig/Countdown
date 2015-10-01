{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CountdownGame.State.Snapshots
       ( Snapshot
       , takeSnapshot
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (sortBy)

import Countdown.Game (Attempt, AttemptsMap, Challange, Player, PlayersMap, PlayerId)
import qualified Countdown.Game as G

import CountdownGame.References
import CountdownGame.State.Definitions (State (..), Round (challange, validTill))

data Snapshot =
  Snapshot
  { goal         :: Maybe Int
  , availableNrs :: [Int]
  , isStartable  :: Bool
  , isRunning    :: Bool
  , secondsLeft  :: Maybe Int
  , scoreBoard   :: [Score]
  } deriving (Generic, Show)

instance ToJSON Snapshot

data Score =
  Score
  { name       :: Text
  , score      :: Int
  , value      :: Maybe Int
  , difference :: Maybe Int
  , formula    :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON Score

takeSnapshot :: Bool -> State -> IO Snapshot
takeSnapshot isAdmin state = do
  rd <- readRef id $ currentRound state
  atts <- readRef id $ playerAttempts state
  ps <- readRef id $ players state
  ready <- readRef isJust $ nextChallange state
  now <- getCurrentTime
  let goal = G.targetNumber . challange <$> rd
      nrs = maybe [] (G.availableNumbers . challange) rd
      till = rd >>= validTill
      secs = (`diffUTCTime` now) <$> till
      run = isJust rd && fromMaybe (-1) secs > 0
      sc = calculateScore (not run && isAdmin) goal ps atts
  return $ Snapshot goal nrs (not run && ready) run (truncate <$> secs) sc

calculateScore :: Bool -> Maybe Int -> PlayersMap -> AttemptsMap -> [Score]
calculateScore _ Nothing ps _ =
  map (\(_,nick) -> Score nick 0 Nothing Nothing Nothing) . M.toList $ M.map G.nickName ps
calculateScore inclFormula (Just g) ps gm =
  sortBy (compare `on` (negate . score)) scores
  where
    scores = map assocGuess . M.toList $ M.map G.nickName ps
    assocGuess (pid, nick) = Score nick scr val diff form
      where diff  = attmp >>= G.difference
            val   = attmp >>= G.value
            scr   = maybe 0 G.score $ attmp
            form  = if inclFormula then G.formula <$> attmp else Nothing
            attmp = M.lookup pid gm

