module ExFromWhile where

import Prelude hiding (fromWhile, fromFor, fromTo, fromToThat)
import Data.List hiding (fromWhile, fromFor, fromTo, fromToThat)
import Data.Char hiding (fromWhile, fromFor, fromTo, fromToThat)


fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
-- fromWhile m n xs = takeWhile n (drop m xs)
fromWhile m n = takeWhile n . drop m

fromFor :: Int -> Int -> [a] -> [a]
fromFor m n = take n . drop m

fromTo :: Int -> Int -> [a] -> [a]
-- fromTo m n = take (n - m + 1) . drop m
fromTo m n = fromFor m (n - m +1) -- a composição fica 'escondida' no fromFor

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
fromToThat m n p = filter p . fromTo m n