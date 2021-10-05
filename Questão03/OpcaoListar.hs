{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module OpcaoListar where


import Contatos

imprimir:: [Contatos] -> String
imprimir [] = "Fim da sua lista de contatos" 
imprimir (x:xs) = imprimirContato(x) ++ imprimir xs--Recursão da lista 

{-Separa os componentes dos contatos e imprime separadamente-}
imprimirContato :: Contatos -> [Char]
imprimirContato (Contatos c n t e) = ("Nome = " ++ c) ++ ("  | CPF = " ++ n) ++ ("  |  Email = " ++ t )++ ("  |  Telefone = " ++ e ) ++ "\n"