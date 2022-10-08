module Talvez where

firsThat :: (a -> Bool) -> [a] -> Maybe a
firsThat _ [] = Nothing
firsThat p (x:xs) 
    | p x = Just x
    | otherwise = firsThat p xs

firsThat' :: (a -> Bool) -> [a] -> Maybe a
firsThat' p = safeHead . filter p

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat _ _ [] = Nothing
isGoodFirstThat f g (x:xs) = 
    if g x
    then Just $f x
    else isGoodFirstThat f g xs
