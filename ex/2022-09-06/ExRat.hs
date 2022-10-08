module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

-- define Rat:
data Rat = Rat Integer Integer

instance Show Rat where
    show (Rat m n) = show m ++ "/" ++ show n

instance Eq Rat where
    Rat x y == Rat m n = x * n == y * m 

instance Num Rat where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Ord Rat where
    compare = undefined

rat :: Integer -> Integer -> Rat
rat x y 
    | y == 0 = error "division by zero is undefined"
    | otherwise = Rat x y 

(//) :: Rat -> Rat -> Rat
(//) = undefined

denominator :: Rat -> Integer
denominator (Rat x y) = y

numerator :: Rat -> Integer
numerator (Rat x y) = x