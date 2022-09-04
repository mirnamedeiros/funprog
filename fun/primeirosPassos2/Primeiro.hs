module Primeiro where
-- Haskell uma linguagem declarativa, sendo assim, cada módulo é uma coleção de definições/declarações

errado :: [Char] -- aconselhado definir o tipo antes
errado = "not good"

five :: Integer
five = 5

addFive :: Integer -> Integer
-- addFive five = 2 + five (errado): pois acontece shadowing das "variaveis", que ficam disponíveis apenas em SUAS funções
addFive x = 5 + x

f :: Integer -> Integer
f x = addFive x + addFive 12

sempreSix :: Integer -> Integer
-- sempreSix x = 6
sempreSix _ = 6

-- OBSs: it guarda a ultima coisa calculada
    --   addFive é do tipo Integer -> Integer, mas addFive 3 é do tipo Integer
    --   Haskell foca na parte mais esquerda/externa da função
    --   será preciso dizer qual a associatividade e precedência das funções (infixl 7 - operador infixo left e 7 de prioridade)
                -- dá pra ver com info (função)
                -- ex: 3 + 8 * 2 * 9 --> 3 + ((8 * 2) * 9) 
                
