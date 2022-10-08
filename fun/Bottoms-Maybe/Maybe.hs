module Maybe where

import Prelude hiding (Maybe (..))

-- o maybe serve para localizar no código possíveis 'booms', 
-- sem precisar mostrar uma mensagem de erro

data Maybe a =  Nothing
            | Just a
            deriving (Show, Eq)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x