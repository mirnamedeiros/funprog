module ExSet
    ( Set
    , empty
    , singleton
    , fromList
    , toList
    , powerSet
    , insert
    , delete
    , member
    , notMember
    , null
    , size
    , isSubsetOf
    , isProperSubsetOf
    , disjoint
    , pairwiseDisjoint
    , union
    , inter
    -- , (\\)
    , unions
    , inters
    , cartesianProduct
    , disjointUnion
    , filter
    , partition
    , map
    ) where

import qualified Data.List as L
import MySort as S

data Set a = Set [a]
    
-- CAUTION: you may need to add constraints to your types and instances!

instance  (Eq a) =>  Eq (Set a) where
    xs == ys  = (size xs == size ys) && (toList xs == toList ys)

instance (Show a) => Show (Set a) where
    show (Set xs) = show (toList (Set xs))

-- smart constructor
set :: [a] -> Set a
set = fromList

empty :: Set a -> Bool
empty (Set []) = True
empty _ = False

singleton :: a -> Set a
singleton x = set [x]

fromList :: [a] -> Set a
fromList xs = (Set xs)
-- usar foldr

toList :: Set a -> [a]
toList (Set xs) = xs

powerSet :: Set a -> Set (Set a)
powerSet = undefined

insert :: (Eq a) => a -> Set a -> Set a
insert x (Set ys) = (Set (insertList x ys))

insertList :: (Eq a) => a -> [a] -> [a] 
insertList x ys | elem x ys = ys 
                | otherwise = (x:ys)

delete :: (Eq a) => a -> Set a -> Set a
delete x (Set xs) = Set (deleteList x xs)

deleteList :: (Eq a) => a -> [a] -> [a]
deleteList x [] = []
deleteList x (y:ys) | x == y    = deleteList x ys 
                    | otherwise = y : deleteList x ys 

member :: (Eq a) => a -> Set a -> Bool
member _ (Set []) = False
member x (Set(y:ys)) | x == y    = True 
                     | otherwise = member x (Set ys)

notMember :: (Eq a) => a -> Set a -> Bool
notMember x (Set ys) = not (member x (Set ys))

null' :: Set a -> Bool
null' (Set []) = True
null' _ = False

size :: Integral i => Set a -> i
size (Set []) = 0
size (Set (_:xs)) = 1  + size (Set xs)

isSubsetOf :: (Eq a) => Set a -> Set a -> Bool
isSubsetOf xs'@(Set (xs)) ys'@(Set (ys)) = (all (== True) (map (\w -> member w ys') xs)) 
                                        && (size xs' <= size ys')
    
isProperSubsetOf :: (Eq a) => Set a -> Set a -> Bool
isProperSubsetOf xs'@(Set (xs)) ys'@(Set (ys)) = (all (== True) (map (\w -> member w ys') xs)) 
                                              && (size xs' < size ys')

disjoint :: (Eq a) => Set a -> Set a -> Bool
disjoint xs'@(Set (xs)) ys'@(Set (ys)) = not (isSubsetOf xs' ys')

pairwiseDisjoint :: Set (Set a) -> Bool
pairwiseDisjoint = undefined

union :: (Eq a) => Set a -> Set a -> Set a
union (Set []) (Set ys) = (Set ys)
union (Set ys) (Set []) = (Set ys)
union (Set (x:xs)) (Set (y:ys)) 
    | x == y    = (Set (x : toList (union (Set xs) (Set ys))))
    | otherwise = (Set (x:y: toList (union (Set xs) (Set ys))))

inter :: (Eq a) =>  Set a -> Set a -> Set a
inter (Set []) _ = (Set [])
inter _ (Set []) = (Set [])
inter (Set (x:xs)) ys'@(Set (ys)) 
    | member x ys' = (Set (x : toList(inter (Set xs) ys') ))
    | otherwise    = (Set (toList(inter (Set xs) ys')))

-- relative complement (set difference)
setminus :: (Eq a) => Set a -> Set a -> Set a
setminus (Set []) _ = (Set [])
setminus (Set xs) (Set []) = (Set xs)
setminus (Set (x:xs)) ys'@(Set (ys)) 
    | member x ys' = (Set (toList(setminus (Set xs) ys')))
    | otherwise    = (Set (x : toList(setminus (Set xs) ys')))

-- (\\) = setminus
-- infixr 5 \\

unions :: Set (Set a) -> Set a
unions = undefined

inters :: (Eq a) => (Set (Set a)) -> Set a
inters = undefined
-- inters = foldable foldl inter (Set [])

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct = undefined

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion = undefined

filter':: (a -> Bool) -> Set a -> Set a
filter' p (Set []) = (Set [])
filter' p (Set (x:xs))
    | p x = (Set (x: filter p xs))
    | otherwise = (Set (filter p xs))

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition = undefined

map' :: (a -> b) -> Set a -> Set b
map' f (Set []) = (Set [])
map' f (Set (x:xs)) = (Set (f x : toList (map' f (Set xs))))

