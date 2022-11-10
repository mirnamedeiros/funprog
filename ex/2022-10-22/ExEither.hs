module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left x:xs) = x : lefts xs
lefts (_:xs) = lefts xs

-- using list comprehension
-- lefts x = [ a | Left a <- x ]

rights :: [Either a b] -> [b]
-- rights [] = []
-- rights (Right x:xs) = x : rights xs
-- rights (_:xs) = rights xs

-- using list comprehension
rights x = [ a | Right a <- x ]

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs = (lefts xs, rights xs)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
