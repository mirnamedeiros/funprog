module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($), curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P hiding (flip, reverse, drop)
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head [] = error "empty list has no head"
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "empty list has no tail"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
lenght (_:xs) = 1 + lenght xs 

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 0
product (x:xs) = x + product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = snoc x (reverse xs)     

(++) :: [a] -> [a] -> [a]
(++) = undefined

-- right-associative for performance!
-- (what?!)
infixr 5 ++

snoc :: a -> [a] -> [a]
snoc y [] = [y]
snoc y (z:zs) = z : snoc y zs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

flip :: (a -> b -> c) -> (b -> a -> c) 
flip f x y = f y x

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "empty list"
minimum [x] = x
minimum (x:xs) = if x < m then x else m
    where m = minimum xs

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [x] = x
maximum (x:xs) = if x > m then x else m
    where m = maximum xs

take :: Int -> [a] -> [a]
take _ [] = []
take 0 xs = []
take n (x:xs) = x : (take (n-1) xs)

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (_:xs) = drop (n-1) xs

-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

