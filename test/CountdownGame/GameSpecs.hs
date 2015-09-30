{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.GameSpecs (main, spec) where

import Test.Hspec
import CountdownGame.Game

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "wenn ein Spieler einen Vorschlag einreicht" $ do
    let rp = RoundParam 765 [1,3,7,10,25,25,50]
    context "und dabei eine valide Formel nur mit den gegebenen Zahlen benutzt" $ do
      let playerGuess = "7*25 + 10*50"
          guess = guessFromFormula rp playerGuess
      it "wird die Formel uebernommen" $ do
        guessFormula guess `shouldBe` playerGuess
      it "wird der Wert gesetzt" $ do
        guessValue guess `shouldBe` Just 675
      it "wird die Differenz zum Zielwert gebildet" $ do
        guessDifference guess `shouldBe` Just 90
      it "ist die Info 'OK'" $ do
        guessInfo guess `shouldBe` "OK"
    context "und die Formel einen Syntaxfehler enthaelt" $ do
      let playerGuess = "7*25 +"
          guess = guessFromFormula rp playerGuess
      it "wird die Formel uebernommen" $ do
        guessFormula guess `shouldBe` playerGuess
      it "wird der Wert nicht gesetzt" $ do
        guessValue guess `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        guessDifference guess `shouldBe` Nothing
      it "ist die Info 'Syntaxfehler in Formel'" $ do
        guessInfo guess `shouldBe` "Syntaxfehler in Formel"
    context "und die Formel nicht den Regeln entspricht (Teilterm negativ)" $ do
      let playerGuess = "7*(3-10)"
          guess = guessFromFormula rp playerGuess
      it "wird die Formel uebernommen" $ do
        guessFormula guess `shouldBe` playerGuess
      it "wird der Wert nicht gesetzt" $ do
        guessValue guess `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        guessDifference guess `shouldBe` Nothing
      it "ist die Info 'Formel enthaelt ungueltige Terme'" $ do
        guessInfo guess `shouldBe` "Formel enthaelt ungueltige Terme"
    context "die Formel nicht den Regeln entspricht (Teilen durch 0)" $ do
      let playerGuess = "7/(25-25)"
          guess = guessFromFormula rp playerGuess
      it "wird die Formel uebernommen" $ do
        guessFormula guess `shouldBe` playerGuess
      it "wird der Wert nicht gesetzt" $ do
        guessValue guess `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        guessDifference guess `shouldBe` Nothing
      it "ist die Info 'Formel enthaelt ungueltige Terme'" $ do
        guessInfo guess `shouldBe` "Formel enthaelt ungueltige Terme"
    context "und die Formel nicht vorgegebene Zahlen enthaelt" $ do
      let playerGuess = "7*5"
          guess = guessFromFormula rp playerGuess
      it "wird die Formel uebernommen" $ do
        guessFormula guess `shouldBe` playerGuess
      it "wird der Wert nicht gesetzt" $ do
        guessValue guess `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        guessDifference guess `shouldBe` Nothing
      it "ist die Info 'Formel darf gegebene Zalhen verwenden'" $ do
        guessInfo guess `shouldBe` "Formel darf gegebene Zalhen verwenden"
    context "und die Formel vorgegebene Zahlen zu oft enthaelt" $ do
      let playerGuess = "25+25*25"
          guess = guessFromFormula rp playerGuess
      it "wird die Formel uebernommen" $ do
        guessFormula guess `shouldBe` playerGuess
      it "wird der Wert nicht gesetzt" $ do
        guessValue guess `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $ do
        guessDifference guess `shouldBe` Nothing
      it "ist die Info 'Formel darf gegebene Zalhen verwenden'" $ do
        guessInfo guess `shouldBe` "Formel darf gegebene Zalhen verwenden"
        
