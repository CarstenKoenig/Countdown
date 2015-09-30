{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Countdown.ParserSpec as Parser
import qualified CountdownGame.GameSpecs as Game


main :: IO ()
main = hspec $ do
  Parser.spec
  Game.spec
