module Tipos where
import System.Posix (DL(Next))
-- dentro de um definição a ordem das equações importam, 
--          mas dentro do módulo a ordem das definições não importam

-- Integral engloba Int e Integer

-- typeclasses: Num, Fractional, Integral

-- primitive types: Char, Int, Integer, Float, Double

-- types: [Char]; (Int, Int); Int -> Int

-- data: 5, av, [], (,), Mon
-- []; (,); (->)

data Weekday = Mon 
            | Tue 
            | Wed 
            | Thu 
            | Fri 
            | Sat 
            | Sun
            deriving (Show)

nextDay :: Weekday -> Weekday
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

nextWorkingDay :: Weekday -> Weekday 
nextWorkingDay Mon = Tue
nextWorkingDay Tue = Wed
nextWorkingDay Wed = Thu
nextWorkingDay Thu = Fri
nextWorkingDay Fri = Mon
nextWorkingDay Sat = Mon
nextWorkingDay Sun = Mon

nextWorkingDay' :: Weekday -> Weekday 
nextWorkingDay' Mon = Tue
nextWorkingDay' Tue = Wed
nextWorkingDay' Wed = Thu
nextWorkingDay' Thu = Fri
nextWorkingDay' _ = Mon

nextWorkingDay'' :: Weekday -> Weekday 
nextWorkingDay'' Fri = Mon
nextWorkingDay'' Sat = Mon
nextWorkingDay'' Sun = Mon
nextWorkingDay'' x = nextDay x

-- nextWorkingDay' (nextDay (nextWorkingDay Mon)) :: Weekday
