module Towards where

import Data.Char (toUpper)

scream :: [Char] -> [Char]
scream "" = ""
scream (x:xs) = toUpper x : scream xs

squareAll :: [Int] -> [Int]
squareAll [] = []
-- squareAll (x:xs) =  x^2 : squareAll xs
squareAll (x:xs) = (^2) x : squareAll xs
