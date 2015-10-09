{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module CountdownGame.State.Rounds
       ( current
       , startNext
       )where

import GHC.Generics (Generic)

import Control.Concurrent.Async (Async, async, wait, poll, asyncThreadId, waitBoth)
import Control.Concurrent (forkIO, threadDelay, ThreadId)

import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')
import Data.List (sortBy)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)

import CountdownGame.State.Challanges
import CountdownGame.References
import CountdownGame.State.Definitions (State (currentRound), Round (Round))
import CountdownGame.Database (insertChallange)

import Countdown.Game (Attempt, AttemptsMap, Challange)
import qualified Countdown.Game as G

startNext :: State -> IO ()
startNext state = do
  next <- getNext state
  time <- (30 `addUTCTime`) <$> getCurrentTime
  case next of
    Just n -> do
      chId <- insertChallange n
      let r = Round n chId (Just time)
      modifyRef (const (Just r, ())) (currentRound state)
      startGeneration state
    Nothing -> return ()

current :: State -> IO (Maybe Round)
current = readRef id . currentRound

-- $doc
-- Es gibt zwei Zust�nde:
--
--   - warten auf den Begin der n�chsten Runde
--   - die Runde l�uft
--
-- Beide Zust�nde laufen nur eine bestimmte Zeitland und
-- wechseln dann in den jeweils anderen Zustand
--
-- W�hrend des Wartens wird zus�tzlich die Challange f�r die n�chste
-- Runde berechnet und die Ergebnise der letzen Runde (falls vorhanden)
-- sind verf�gbar.
-- Am Ende dieser Wartezeit ist ist die n�chste Challange verf�gbar
--
-- W�hrend einer Runde ist die aktuelle Challange verf�gbar und die
-- Spieler k�nnen ihre Attempts schicken (die gespeichert werden)
-- Am Ende dieser Phase sind die Ergebnise der Runde verf�gbar

startGameLoop :: SpielParameter -> IO (Reference Phasen)
startGameLoop params = do
  ref <- createRef Start
  _   <- forkIO $ gameLoop params [] ref
  return ref

gameLoop :: SpielParameter -> Ergebnisse ->  Reference Phasen -> IO ()
gameLoop params ltErg phase = do
  warten <- wartePhase params ltErg
  modifyRef (const (warten, ())) phase
  aufg <- wait . naechsteChallange $ warten
  runde <- rundenPhase params aufg
  modifyRef (const (runde, ())) phase
  erg <- wait . ergebnisse $ runde
  gameLoop params erg phase
  
wartePhase :: SpielParameter -> Ergebnisse -> IO Phasen
wartePhase params ltErg = do
  wartenBis <- (warteZeit params `addUTCTime`) <$> getCurrentTime
  start     <- async $ warteStart wartenBis
  return $ WartePhase wartenBis ltErg start

warteStart :: UTCTime -> IO Challange
warteStart startZeit  = do
  neue  <- async G.generateChallange
  warte <- async $ warteBis startZeit
  (chal, _) <- waitBoth neue warte
  return chal

rundenPhase :: SpielParameter -> Challange -> IO Phasen
rundenPhase params chal = do
  rundeBis <- (rundenZeit params `addUTCTime`) <$> getCurrentTime
  vers     <- createRef M.empty
  erg      <- async $ warteErgebnise rundeBis chal vers
  return $ RundePhase rundeBis chal vers erg

warteErgebnise :: UTCTime -> Challange -> Versuche -> IO Ergebnisse
warteErgebnise endeZeit aufg refVers = do
  warteBis endeZeit
  readRef (berechneErgebnise aufg) refVers

warteBis :: UTCTime -> IO ()
warteBis zeit = do
  now <- getCurrentTime
  if now < zeit then do
    threadDelay 250000
    warteBis zeit
  else return ()
  

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

berechneErgebnise :: Challange -> AttemptsMap -> Ergebnisse
berechneErgebnise chal attMap =
  sortBy (compare `on` (negate . score)) scores
  where
    scores = map calcScore . M.toList $ attMap
    calcScore (_, att) =
      Ergebnis (G.nickName $ G.fromPlayer att) (G.score att) (G.value att) (G.difference att) (G.formula att)


