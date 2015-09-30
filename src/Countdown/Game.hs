{-# LANGUAGE OverloadedStrings #-}

module Countdown.Game
       ( PlayerId
       , Player (..)
       , Attempt (..)
       , Challange (..)
       , attempt
       , attemptFromFormula
       )where

import Countdown.Game.Players (PlayerId, Player (..))
import Countdown.Game.Attempts (Attempt(..), attempt, attemptFromFormula)
import Countdown.Game.Challanges (Challange (..))
