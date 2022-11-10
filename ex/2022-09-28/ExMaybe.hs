module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes x = [ a | Just a <- x]

fromJust :: Maybe a -> a
fromJust (Just a) = a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just x) = x

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False 

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing  _ = False 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = (Just x)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = map fromJust . filter isJust . map f
-- mapMaybe _ []     = []
-- mapMaybe f (x:xs) =
--     let rs = mapMaybe f xs in
--     case f x of
--         Nothing -> rs
--         Just r  -> r:rs

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

maybeApply :: Maybe (a -> a) -> a -> a
maybeApply Nothing x = x
maybeApply (Just f) x = f x

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith fs xs = zipWith maybeApply fs xs