module FilterConcat where 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = concat . map box
    where box x
            | p x       = [x]
            | otherwise = []