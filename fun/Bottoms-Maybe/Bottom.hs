module Bottom where

data Nat = Zero | Succ Nat
    deriving (Eq)

instance Show Nat where

    show Zero = "O"
    show (Succ x) = 'S' : show x

-- Bottom : quando a função não retorna nenhum valor normal do tipo que foi definido (loops).
-- No mundo dos Nats, temos O, SO, SSO, SSSO... 
-- mas também temos _|_,e por ser um Nat, S_|_, SS_|_, SSS_|_ ...
-- S_|_ + SS0 = S(SS_|_ + SO) = SSSS_|_, ou seja, SS_|_ é pelo menos 2 (SSO)

-- quando há chance da função retornar um valor bottom, a função é parcial
-- ex: head [a] -> a, onde head [] = _|_

bottom :: Nat
bottom = bottom

(<+>) :: Nat -> Nat -> Nat
(<+>) x Zero = x 
(<+>) x (Succ y) = Succ((<+>) x y)

atLeast :: Nat -> Nat
atLeast Zero = bottom
atLeast (Succ n) = Succ $atLeast n

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat x = Succ (toNat (x - 1))

-- já tem um tipo e esse tipo tem apenas um único construtor
-- com newtype não é possível diferenciar entre os bottoms
newtype Angle = Angle Int
-- ex: 2 e 362 são o mesmo ângulo, mas nos inteiros são diferentes
-- _|_ - _|_, int 0 - angle 0, int 1 - angle 1, mas ? - angle_|_
-- no newtype, angle é uma cópia fiel de int
-- mesmo que seja A_|_, tudo será criado a partir do Angle  