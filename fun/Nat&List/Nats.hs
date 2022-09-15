module Nats where

data Nat = Zero 
        | Succ Nat
        deriving( Eq )

plus :: Nat -> Nat -> Nat
plus n Zero     = n
plus n (Succ m) = Succ (plus n m)

instance (Show Nat) where
    show Zero     = "O"
    show (Succ n) = "S" ++ show n

len :: [a] -> Nat
len[]     = Zero
len(x:xs) = Succ (len xs)

rev :: [a] -> [a]
rev []     = []
-- [x] pq (rev xs) é lista
rev (x:xs) = xs' ++ [x]
        where xs' = rev xs

-- casa apenas com listas de tamanho > 1
atLeastTwo :: [a] -> Bool
atLeastTwo []  = False
atLeastTwo [_] = False
atLeastTwo _   = True
-- atLeastTwo (x:xs) = 
--         case xs of
--                 [] -> 
--                         False
--                 (y:ys) -> 
--                         True

firstTwo :: [a] -> (a,a)
firstTwo []    = error "nao tem 2 primeiros"
firstTwo [_]   = error "nao tem 2 primeiros"
-- firstTwo(x:xs) = (x, head xs)
firstTwo(x:y:_) = (x, y)

describe :: [a] -> String
describe xs 
        | l == 0 = "empty"
        | l < 3  = "small"
        | l < 6  = "big"
        | otherwise      = "too big"
        where l = length xs