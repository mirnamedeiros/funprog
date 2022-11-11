module ExUsing where

import Prelude hiding
    ( filter
    )

type Pred a = (a -> Bool)

-- using concat
filter :: Pred a -> [a] -> [a]
filter p = concat . map box
    where box x
            | p x       = [x]
            | otherwise = []

-- using zipWith
sorted :: Ord a => [a] -> Bool
sorted xs'@(x:xs) = and (zipWith (<=) xs' xs)
sorted _         = True

-- using zipWith
fibs :: Integral i => [i]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

