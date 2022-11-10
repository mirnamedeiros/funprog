module ExDrunk
    ( atIndices
    , everyOther
    , disjoint
    , stretch
    , drunk
    ) where

-- example:
-- atIndices [1,4,5] "Tchauzinho"
-- = "cuz"
atIndices :: Integral i => [i] -> [a] -> [a]
atIndices (i:is) xs = findIndices i xs : atIndices is xs
atIndices [] _ = []

findIndices :: (Eq a, Num a) => a -> [b] -> b
findIndices _ [] = error "invalid index"
findIndices 0 (x:_) = x
findIndices i (_:xs) = findIndices (i-1) xs

-- example:
-- everyOther 2 "Hello There"
-- = "HloTee"
everyOther :: Integral i => i -> [a] -> [a]
everyOther _ [] = []
everyOther 0 (xs) = xs
everyOther i (x:xs) = undefined

-- examples:
-- disjoint [1,5,9] [2 .. 6]
-- = False
-- disjoint [1,5,9] [2,4 ..]
-- = True
-- ASSUMPTIONS FOR disjoint xs ys:
--   xs and ys are sorted
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint xs ys = all (==False) (map (\w -> belong w ys) xs)

belong :: Ord a => a -> [a] -> Bool
belong _ [] = False
belong x (y:ys) 
            | x == y    = True 
            | otherwise = belong x ys

-- example:
-- stretch 3 "Gustavo"
-- = "GGGuuussstttaaavvvooo"
stretch :: Integral i => i -> [a] -> [a]
stretch = undefined

-- example:
-- drunk 3 "Gustavo"
-- = "GusGtuasvtoavo"
-- drunk 5 "Gustavo"
-- = "GustaGvuostavo"
-- To understand these string, either get drunk or look at the markings:
--       , , , , ,,,
--   "GusGtuasvtoavo"
--    ''' ' ' ' '
--         , , ,,,,,
--   "GustaGvuostavo"
--    ''''' ' '
drunk :: Integral i => i -> [a] -> [a]
drunk = undefined

