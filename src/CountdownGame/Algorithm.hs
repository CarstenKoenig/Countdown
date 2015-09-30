{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Algorithm
       ( Operand (..)
       , Expression (..)
       , isValidOp
       , apply
       , values
       , eval
       , isSubsetOf
       )where

import Data.List (delete)

data Operand
  = Add | Sub | Mul | Div
  deriving (Show, Eq, Ord)

isValidOp :: (Ord a, Integral a) => Operand -> a -> a -> Bool
isValidOp Add _ _ = True
isValidOp Sub x y = x > y
isValidOp Mul _ _ = True
isValidOp Div x y = x `mod` y == 0

apply :: Integral a => Operand -> a -> a -> a
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

data Expression
  = Value Int
  | Apply Operand Expression Expression
  deriving (Show, Eq)

values :: Expression -> [Int]
values (Value n)     = [n]
values (Apply _ x y) = values x ++ values y

eval :: Expression -> [Int]
eval (Value n)      = [ n | n > 0 ]
eval (Apply op x y) = [ apply op a b | a <- eval x, b <- eval y, isValidOp op a b ]

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf _ [] = False
isSubsetOf (x:xs) ys = x `elem` ys && xs `isSubsetOf` (delete x ys)
