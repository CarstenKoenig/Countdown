{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Countdown.Game.Attempts
       ( Attempt (..)
       , AttemptsMap
       , attempt
       , attemptFromFormula
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Countdown.Expressions (values, eval)
import Countdown.Parser (tryParse)
import Countdown.Lists (isSubsetOf)

import Countdown.Game.Players (PlayerId)
import Countdown.Game.Challanges (Challange(..))

type AttemptsMap = Map PlayerId Attempt

data Attempt =
  Attempt
  { formula    :: Text
  , value      :: Maybe Int
  , difference :: Maybe Int
  , info       :: Text
  } deriving (Generic, Show)

instance ToJSON Attempt

attempt :: Challange -> Text -> PlayerId -> AttemptsMap -> (AttemptsMap, Attempt)
attempt ch txt pid aMap =
  let att = attemptFromFormula ch txt
  in (M.insert pid att aMap, att)

attemptFromFormula :: Challange -> Text -> Attempt
attemptFromFormula ch txt =
  case tryParse txt of
    Nothing -> Attempt txt Nothing Nothing "Syntaxfehler in Formel"
    Just ex -> if values ex `isSubsetOf` availableNumbers ch
               then mapValue $ eval ex
               else Attempt txt Nothing Nothing "Formel darf gegebene Zahlen verwenden"
  where
    mapValue []  = Attempt txt Nothing Nothing "Formel enthaelt ungueltige Terme"
    mapValue [v] = Attempt txt (Just v) (Just $ dif v) "OK"
    mapValue _   = error "kein eindeutiges Ergebnis"
    dif v' = abs (targetNumber ch - v')
