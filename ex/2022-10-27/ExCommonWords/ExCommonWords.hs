module ExCommonWords
    ( commonWords
    ) where

-- commonWords 4 "Define the function commonWords,
-- which should receive an Int n and a text and
-- return some string describing the n most common words
-- found in the input text."
--
-- should return something like:
--
-- "the: 3\nand: 2\nn: 2\ntext: 2\n"

-- Use everything you've learnt so far to make your
-- program elegant and nice!

-- do not alter this import
import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..), Show(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Data.Char as C
    -- probably you will not need all of them:
    ( isAlpha , isSymbol , isPunctuation , isSpace , toLower )

-- Your imports should go here.
-- Might want to make them qualified;
-- import only what you need from each module!

-- you may only import further entities that YOU have
-- already defined in other modules:

import MySort ( sort )

-- **REPLACE** Data.List by your own home-made ExList!
import ExList as L
    -- feel free to remove and/or add entities:
    ( (++)
    , break
    , length
    , span
    , dropWhile
    , concat
    , reverse
    , map
    , filter
    , take
    ) 

-- Let's start with some type synonyms you might want to use:

type Text = String
type Word = String

-- On with the functions now:

commonWords
    :: Int     -- how many common words
    -> Text    -- the input text (book, poem, whatever)
    -> String  -- the output string with the results

commonWords n = 
    concat . map showRun . take n .
    sortRuns . countRuns .
    sortWords . words .
    map C.toLower

showRun :: (Int,Word) -> String
showRun (n,w) = w ++ ": " ++ show n ++ "\n" 

-- if you think this makes your code more readable...
type Run = [(Int,Word)]

countRuns :: [Word] -> [(Int,Word)]
countRuns [] = [] 
countRuns (w:ws) = (1 + length us, w) : countRuns vs 
    where 
        (us, vs) = span (==w) ws 

sortWords :: [Word] -> [Word]
sortWords = sort  

sortRuns :: [(Int,Word)] -> [(Int,Word)]
sortRuns = reverse . sort     

-- tuplesToList :: [(Int,Word)] -> [[a]]
-- tuplesToList (xs) = [[x,y] | (x,y) <- xs ]

words :: Text -> [Word]
words t 
    |t' == "" = []
    |otherwise = word : words rest
        where t' = dropWhile C.isSpace t
              (word, rest) = break C.isSpace t'