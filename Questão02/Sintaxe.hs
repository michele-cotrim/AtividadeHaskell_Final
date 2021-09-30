{-Elaborar um programa em Haskell para solicitar ao usuário (E/S) uma expressão aritmética,
verificar o balanceamento dos parenteses e imprimir “sintaxe válida” ou “sintaxe inválida”. A
validação da expressão aritmética deverá ser realizada utilizando um tipo abstrato de dados Pilha.


Contar os "(" e os ")" e verificar se são iguais
-}

module Sintaxe where

import Stack
  
contagem :: String -> Stack Char -> String
contagem [] stack = if isEmpty stack then "Sintaxe valida" else "Sintaxe Invalida"
contagem (x:s) stack
  |x == '(' = contagem s (push x stack) 
  |x == ')' = contagem s (pop stack)
  |otherwise = contagem s (stack)



main :: IO ()
main = do putStrLn ("Bota ai")
          coisa <-getLine 
          putStrLn(contagem coisa stackNew)
