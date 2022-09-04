module Av where

-- av :: Integer -> Integer -> Integer
-- av a b = div (a + b) 2
av :: Fractional a => a -> a -> a -- Fractional is a typeclass não um type
av a b = (a + b) / 2