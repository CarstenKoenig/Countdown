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
       , Guess (guessFormula, guessValue, guessDifference, guessInfo)
       , guessFromFormula
       , setGuess
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
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (sortBy)

import Countdown.Expressions (eval, values)
import Countdown.Lists (isSubsetOf)
import Countdown.Parser (tryParse)

type PlayerId = Integer

data State =
  State
  { currentRound  :: Reference (Maybe Round)
  , nextRound     :: Reference (Maybe RoundParam)
  , players       :: Players
  , playerGuesses :: Guesses
  }

-- ** Snapshots: DTO f�r die Clients

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
instance FromJSON Snapshot

takeSnapshot :: State -> IO Snapshot
takeSnapshot state = do
  rd <- readRef id $ currentRound state
  guesses <- readRef id $ playerGuesses state
  ps <- readRef id $ players state
  ready <- readRef isJust $ nextRound state
  now <- getCurrentTime
  let g = target . params <$> rd
      nrs = maybe [] (numbers . params) rd
      till = rd >>= validTill
      secs = (`diffUTCTime` now) <$> till
      run = isJust rd && fromMaybe (-1) secs > 0
      score = calculateScore (not run) g ps guesses
  return $ Snapshot g nrs (not run && ready) run (truncate <$> secs) score

calculateScore :: Bool -> Maybe Int -> PlayersMap -> GuessesMap -> [(Text, Maybe Int, Maybe Text)]
calculateScore _ Nothing ps _ = map (\(_,nick) -> (nick, Nothing, Nothing)) . M.toList $ M.map nickName ps
calculateScore running (Just g) ps gm = sortBy (compare `on` (\(_,a,_) -> a)) scores
  where
    scores = map assocGuess . M.toList $ M.map nickName ps
    assocGuess (pid, nick) = (nick, diff, form)
      where diff  = guess >>= guessDifference
            form  = if running then guessFormula <$> guess else Nothing
            guess = M.lookup pid gm

-- ** Spieler-Versuche f�r die aktuelle Runde
    
type Guesses = Reference GuessesMap
type GuessesMap = Map PlayerId Guess
data Guess =
  Guess
  { guessFormula    :: Text
  , guessValue      :: Maybe Int
  , guessDifference :: Maybe Int
  , guessInfo       :: Text
  } deriving (Generic, Show)

instance ToJSON Guess

setGuess :: State -> PlayerId -> Text -> IO (Maybe Guess)
setGuess state pid txt = do
  rp <- readRef (fmap params) $ currentRound state
  case rp of
    Just rp' -> return <$> (modifyRef (assocGuess rp' txt pid) $ playerGuesses state)
    Nothing  -> return Nothing

assocGuess :: RoundParam -> Text -> PlayerId -> GuessesMap -> (GuessesMap, Guess)
assocGuess rp txt pid gm =
  let guess = guessFromFormula rp txt
  in (M.insert pid guess gm, guess)

guessFromFormula :: RoundParam -> Text -> Guess
guessFromFormula rp txt =
  case tryParse txt of
    Nothing -> Guess txt Nothing Nothing "Syntaxfehler in Formel"
    Just ex -> if values ex `isSubsetOf` numbers rp
               then mapValue $ eval ex
               else Guess txt Nothing Nothing "Formel darf gegebene Zahlen verwenden"
  where
    mapValue []  = Guess txt Nothing Nothing "Formel enthaelt ungueltige Terme"
    mapValue [v] = Guess txt (Just v) (Just $ dif v) "OK"
    mapValue _   = error "kein eindeutiges Ergebnis"
    dif v' = abs (target rp - v')

-- ** Spieler

data Player =
  Player
  { nickName :: Text
  , playerId :: PlayerId
  } deriving (Generic, Show)

instance ToJSON Player

type Players = Reference PlayersMap
type PlayersMap = Map PlayerId Player
         
data Round =
  Round
  { params      :: RoundParam
  , validTill   :: Maybe UTCTime
  } deriving (Generic, Show)

instance ToJSON Round

data RoundParam =
  RoundParam
  { target  :: Int
  , numbers :: [Int]
  } deriving (Generic, Show)

instance ToJSON RoundParam

initState :: IO State
initState = do
  ps <- initializePlayers
  emptyR <- emptyRoundState
  emptyP <- emptyRoundParamState
  noGuesses <- Reference <$> newIORef M.empty
  return $ State emptyR emptyP ps noGuesses

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
