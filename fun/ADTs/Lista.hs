module Lista where

data ListInt = Empty 
             | List Int ListInt
             
instance Show ListInt where
    show x = "[" ++ show' x ++ "]"
        where
            show' Empty             = ""
            show' (List x Empty) = show x
            show' (List x y)     = show x ++ "," ++ show' y


hd :: ListInt -> Int
hd Empty = error "Lista vazia nao tem head"
hd (List x y) = x

tl :: ListInt -> ListInt 
tl Empty = error "Lista vazia nao tem tail"
tl (List x y) = y