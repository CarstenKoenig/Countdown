{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Parser
       ( tryParse
       )where

import Data.Text (Text)
import CountdownGame.Algorithm

import Text.Parsec

tryParse :: Text -> Maybe Expression
tryParse = tryParseWith expressionP

type Parser = Parsec Text ()

expressionP :: Parser Expression
expressionP = Value . read <$> many1 digit

tryParseWith :: Parser a -> Text -> Maybe a
tryParseWith p txt =
  case (parse p "" txt) of
    Left _  -> Nothing
    Right a -> Just a
