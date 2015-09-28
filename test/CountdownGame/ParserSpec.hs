{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.ParserSpec (main, spec) where

import Test.Hspec
import CountdownGame.Parser
import CountdownGame.Algorithm

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "beim Evaluieren eines mit tryParse gelesenen Textes" $ do
    it "ergibt '0' [] da 0 nicht positiv ist" $ do
      (eval <$> tryParse "0") `shouldBe` Just []
    it "ergibt '5' einfach 5" $ do
      (eval <$> tryParse "5") `shouldBe` Just [5]
    it "ergibt '99' einfach 99" $ do
      (eval <$> tryParse "99") `shouldBe` Just [99]
