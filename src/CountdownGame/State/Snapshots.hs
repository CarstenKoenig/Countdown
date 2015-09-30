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

import Countdown.Game.Attempts (Attempt, AttemptsMap, difference, formula, attempt)
import Countdown.Game.Challanges (Challange, targetNumber, availableNumbers)
import Countdown.Game.Players (Player, PlayersMap, PlayerId, nickName)

import CountdownGame.References
import CountdownGame.State.Definitions (State (..), Round (challange, validTill))

data Snapshot =
  Snapshot
  { goal         :: Maybe Int
  , availableNrs :: [Int]
  , isStartable  :: Bool
  , isRunning    :: Bool
  , secondsLeft  :: Maybe Int
  , scoreBoard   :: [(Text, Maybe Int, Maybe Text)]
  } deriving (Generic, Show)

instance ToJSON Snapshot

takeSnapshot :: State -> IO Snapshot
takeSnapshot state = do
  rd <- readRef id $ currentRound state
  atts <- readRef id $ playerAttempts state
  ps <- readRef id $ players state
  ready <- readRef isJust $ nextChallange state
  now <- getCurrentTime
  let goal = targetNumber . challange <$> rd
      nrs = maybe [] (availableNumbers . challange) rd
      till = rd >>= validTill
      secs = (`diffUTCTime` now) <$> till
      run = isJust rd && fromMaybe (-1) secs > 0
      score = calculateScore (not run) goal ps atts
  return $ Snapshot goal nrs (not run && ready) run (truncate <$> secs) score

calculateScore :: Bool -> Maybe Int -> PlayersMap -> AttemptsMap -> [(Text, Maybe Int, Maybe Text)]
calculateScore _ Nothing ps _ = map (\(_,nick) -> (nick, Nothing, Nothing)) . M.toList $ M.map nickName ps
calculateScore running (Just g) ps gm = sortBy (compare `on` (\(_,a,_) -> a)) scores
  where
    scores = map assocGuess . M.toList $ M.map nickName ps
    assocGuess (pid, nick) = (nick, diff, form)
      where diff  = guess >>= difference
            form  = if running then formula <$> guess else Nothing
            guess = M.lookup pid gm

