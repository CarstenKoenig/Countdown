{-# LANGUAGE OverloadedStrings #-}

module Countdown.Expressions
       ( Operand (..)
       , Expression (..)
       , isValidOp
       , apply
       , values
       , eval
       , solutions
       , solutions'
       )where

import Countdown.Lists (subbags, notEmptySplit)

data Operand
  = Add | Sub | Mul | Div
  deriving (Eq, Ord)

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
  deriving (Eq)

values :: Expression -> [Int]
values (Value n)     = [n]
values (Apply _ x y) = values x ++ values y

eval :: Expression -> [Int]
eval (Value n)      = [ n | n > 0 ]
eval (Apply op x y) = [ apply op a b | a <- eval x, b <- eval y, isValidOp op a b ]

solution :: Expression -> [Int] -> Int -> Bool
solution e ns n = values e `elem` subbags ns && eval e == [n]

expressions :: [Int] -> [Expression]
expressions []  = []
expressions [n] = [Value n]
expressions ns  = [ Apply op l r | (ls,rs) <- notEmptySplit ns
                                 , l <- expressions ls
                                 , r <- expressions rs
                                 , op <- [Add, Sub, Mul, Div] ]
                  
bruteForceSolutions :: [Int] -> Int -> [Expression]
bruteForceSolutions ns n = [ e | ns' <- subbags ns, e <- expressions ns', eval e == [n]]

-- * schneller: filtere ungültige Sub-Expressions gleich heraus (siehe oben)

type Result = (Expression, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [ (Value n, n) | n > 0 ]
results ns  = [ res | (ls,rs) <- notEmptySplit ns
                    , lx <- results ls
                    , ry <- results rs
                    , res <- combine lx ry ]
  where combine (l,x) (r,y) = [ (Apply op l r, apply op x y) | op <- ops, isValidOp op x y ]
        ops = [ Add, Sub, Mul, Div ]

solutions :: [Int] -> Int -> [Expression]
solutions ns n = [ e | ns' <- subbags ns, (e,m) <- results ns', m == n ]

-- * noch schneller: schränke valide Operationen mit Rechengesetzen ein:

isValidOp' :: (Ord a, Integral a) => Operand -> a -> a -> Bool
isValidOp' Add x y = x <= y
isValidOp' Sub x y = x > y
isValidOp' Mul x y = x /= 1 && y /= 1 && x <= y
isValidOp' Div x y = y /= 1 && x `mod` y == 0

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [ (Value n, n) | n > 0 ]
results' ns  = [ res | (ls,rs) <- notEmptySplit ns
                     , lx <- results' ls
                     , ry <- results' rs
                     , res <- combine lx ry ]
  where combine (l,x) (r,y) = [ (Apply op l r, apply op x y) | op <- ops, isValidOp' op x y ]
        ops = [ Add, Sub, Mul, Div ]

solutions' :: [Int] -> Int -> [Expression]
solutions' ns n = [ e | ns' <- subbags ns, (e,m) <- results' ns', m == n ]


-- * Formatierung der Ausgabe / Show

instance Show Expression where
  show ex = snd $ formatEx 0 ex

formatEx :: Int -> Expression -> (Int, String)
formatEx _ (Value n) = (9, show n)
formatEx prec (Apply op l r)
  | opPrec <= prec = (prec, "(" ++ formatted ++ ")")
  | otherwise     = (prec, formatted)
  where opPrec    = precedence op
        formatted = let (lp, ls) = formatEx opPrec l
                        (_,  rs) = formatEx lp r
                    in  ls ++ show op ++ rs

precedence :: Operand -> Int
precedence Mul = 9
precedence Div = 8
precedence Add = 5
precedence Sub = 4

instance Show Operand where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
