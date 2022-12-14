module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero = "O"
    show (Succ x) = "S" ++ show x

 
instance Eq Nat where
    Zero == Zero = True
    Succ x == Succ y = x == y 
    x == y = False

instance Ord Nat where

    x <= y = min x y == x 
    x >= y = max x y == x 

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min x Zero = Zero
    min Zero y = Zero
    min (Succ x) (Succ y) = Succ (min x y)

    max x Zero = x
    max (Succ x) (Succ y) = Succ (max x y)

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False 

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero = True
even (Succ Zero) = False
even (Succ(Succ x)) = even x

odd :: Nat -> Bool
odd Zero = False
odd (Succ Zero) = True
odd (Succ(Succ x)) = odd x

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) x Zero = x 
(<+>) x (Succ y) = Succ((<+>) x y)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) x Zero = x
(<->) (Succ x) (Succ y) = (<->) x y
(<->) x y = Zero

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) x Zero = Zero
(<*>) x (Succ y) = (<+>) ((<*>) x y) x

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) x Zero = Succ Zero
(<^>) x (Succ y) = (<*>) x ((<^>) x y)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) _ Zero = error "divis??o por zero ?? indefinido"
(</>) x y 
    | x == y = (Succ Zero)
    | x <= y = Zero 
    | otherwise = (<+>) (Succ Zero) ((</>) ((<->) x y) y)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ Zero = error "divis??o por zero ?? indefinido"
(<%>) x y = (<->) x (((<*>) y (((</>) x y))))

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero = (Succ Zero)
factorial (Succ n) = (<*>) (Succ n) (factorial (n))

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _ = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo b (Succ Zero) = Zero
lo b a
    | b == a = (Succ Zero)
    | b > a = Zero
    | otherwise = (<+>) (Succ Zero) (lo b ((</>) a b))

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat x = Succ (toNat (x - 1))

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat (Succ x) = 1 + fromNat x


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = undefined
        | x == 0    = undefined
        | otherwise = undefined

