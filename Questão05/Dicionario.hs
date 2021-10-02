module Dicionario (Dicionario, novo, colocar, ordsim) where

data Dicionario = Vazio | No Dicionario (String, Int) Dicionario deriving Show

novo :: Dicionario
novo = Vazio

colocar :: String -> Dicionario -> Dicionario
colocar str Vazio = No Vazio (str, 1) Vazio
colocar str (No esquerda (ch, vl) direita) | ch < str = No esquerda (ch, vl) (colocar str direita) -- direita
                                        | ch > str = No (colocar str esquerda) (ch, vl) direita -- vou p esquerda
                                        | otherwise = No esquerda (ch, vl + 1) direita 

-- (ch, vl) = chave -> value

ordsim :: Dicionario -> [(String, Int)]
ordsim Vazio = []
ordsim (No esquerda (ch, val) direita) = ordsim esquerda ++ [(ch, val)] ++ ordsim direita
