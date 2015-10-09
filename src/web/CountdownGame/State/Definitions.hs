{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CountdownGame.State.Definitions
       ( SpielParameter (..)
       , Snapshot
       , takeSnapshot
       , Versuche
       , Ergebnis (..), Ergebnisse, berechneErgebnisse
       , State (..)
       , Phasen (..)
       )where

import GHC.Generics (Generic)

import Control.Concurrent.Async (Async)

import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Database.Persist.Sql (ConnectionPool)

import Countdown.Game (Attempt, AttemptsMap, Challange, Player, PlayersMap)
import qualified Countdown.Game as G

import CountdownGame.References

data State =
  State
  { aktuellePhase  :: Reference Phasen
  , connectionPool :: ConnectionPool
  }

data Snapshot =
  Snapshot
  { goal         :: Maybe Int
  , availableNrs :: [Int]
  , isWaiting    :: Bool
  , isRunning    :: Bool
  , secondsLeft  :: Int
  , scoreBoard   :: Ergebnisse
  } deriving (Generic, Show)

instance ToJSON Snapshot

takeSnapshot :: State -> IO Snapshot
takeSnapshot state = do
  now <- getCurrentTime
  readRef (readPhase now) $ aktuellePhase state
  where
    readPhase _ Start = Snapshot Nothing [] False False 0 []
    readPhase n (WartePhase t ergs _) =
      Snapshot Nothing [] True False (bisT n t) ergs
    readPhase n (RundePhase t ch _ _ _) =
      Snapshot (Just $ G.targetNumber ch) (G.availableNumbers ch) False True (bisT n t) []
    bisT n t = max 0 . truncate $ t `diffUTCTime` n

data SpielParameter =
  SpielParameter
  { warteZeit  :: NominalDiffTime
  , rundenZeit :: NominalDiffTime
  }

data Phasen
  = Start
  | WartePhase
    { startNaechsteRunde :: UTCTime
    , letzteErgebnisse   :: Ergebnisse
    , naechsteChallange  :: Async Challange }
  | RundePhase
    { endeRunde       :: UTCTime
    , aufgabe         :: Challange
    , spielerVersuche :: Versuche
    , databaseKey     :: Int64
    , ergebnisse      :: Async Ergebnisse }

type Versuche   = Reference AttemptsMap

type Ergebnisse = [Ergebnis]
data Ergebnis =
  Ergebnis
  { name       :: Text
  , score      :: Int
  , value      :: Maybe Int
  , difference :: Maybe Int
  , formula    :: Text
  } deriving (Generic, Show)

instance ToJSON Ergebnis

berechneErgebnisse :: AttemptsMap -> Ergebnisse
berechneErgebnisse attMap =
  sortBy (compare `on` (negate . score)) scores
  where
    scores = map calcScore . M.toList $ attMap
    calcScore (_, att) =
      Ergebnis (G.nickName $ G.fromPlayer att) (G.score att) (G.value att) (G.difference att) (G.formula att)
