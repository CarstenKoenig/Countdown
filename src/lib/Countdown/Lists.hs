{-# LANGUAGE OverloadedStrings #-}

module Countdown.Lists
       ( isSubsetOf
       )where

import Data.List (delete)

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf _ [] = False
isSubsetOf (x:xs) ys = x `elem` ys && xs `isSubsetOf` (delete x ys)
