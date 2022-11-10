module MySort 
    ( sort
    , msort
    , merge
    , halve
    , isort
    , qsort
    )where

--import qualified Prelude as P

sort :: Ord a => [a] -> [a]
sort = msort

-- sortTuples :: [(Int,String)] -> [(Int,String)]
-- sortTuples ts = map (sortTuple) ts

-- sortTuple :: (Ord a) => (a,a) -> (a,a)
-- sortTuple (x,y) = (min x y, max x y)

msort :: Ord a => [a] -> [a]
msort []  = []
msort [z] = [z]
msort zs  = merge (msort xs) (msort ys)
    where
        (xs,ys) = halve zs

merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge xs'@(x:xs) ys'@(y:ys)
    | x <= y    = x : merge xs ys'
    | otherwise = y : merge xs' ys

halve :: [a] -> ([a],[a])
halve []       = ([],[])
halve [x]      = ([x],[])
halve (x:y:xs) = (x:lxs, y:rxs)
    where
        (lxs,rxs) = halve xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert w [] = [w]
insert w xs'@(x:xs)
    | w < x     = w : xs'
    | otherwise = x : insert w xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (w:ws) = qsort small ++ [w] ++ qsort large
    where
        --small = [ x | x <- xs , x <= w] 
        --large = [ x | x <- xs, x > w]   
        small = filter (< w) ws
        large = filter (> w) ws

sorted :: Ord a => [a] -> Bool
-- sorted (x:y:ys) = x <= y && sorted(y:ys)
sorted xs'@(x:xs) = and (zipWith (<=) xs' xs)
sorted _         = True
