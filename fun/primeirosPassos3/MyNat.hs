module MyNat where

-- Zero e Succ Nat são data constructors
data Nat = Zero | Succ Nat
            deriving (Show)

plus :: Nat -> Nat -> Nat
plus n Zero     = n
plus n (Succ m) = Succ (plus n m)

-- mesma forma de fazer com infix
-- n `plus` Zero     = n
-- n `plus` (Succ m) = Succ (n `plus` m)

(+) :: Nat -> Nat -> Nat
(+) = plus