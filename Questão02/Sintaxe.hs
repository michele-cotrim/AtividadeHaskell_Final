{-Elaborar um programa em Haskell para solicitar ao usuário (E/S) uma expressão aritmética,
verificar o balanceamento dos parenteses e imprimir “sintaxe válida” ou “sintaxe inválida”. A
validação da expressão aritmética deverá ser realizada utilizando um tipo abstrato de dados Pilha.


Contar os "(" e os ")" e verificar se são iguais
-}

module Sintaxe where
import Stack ( Stack, stackNew, push, pop, isEmpty )
import Testes ( assertEqual )

{-Função contagem recebe uma String e uma pilha 
 A função empilha todo "(" que encontrar e a cada ")" que encontrar, a sintaxe é válida se a pilha acabar vazia 
-}
contagem :: String -> Stack Char -> String
contagem [] stack = if isEmpty stack then "Sintaxe valida" else "Sintaxe Invalida"
contagem (x:s) stack
  |x == '(' = contagem s (push x stack)
  |x == ')' = contagem s (pop stack)
  |otherwise = contagem s stack

main :: IO ()
main = do testeValido
          testeValido02
          testeInvalido
          testeInvalido02
          putStrLn ""

testeValido:: IO()
testeValido =
  assertEqual ("Sintaxe valida") (contagem "((((()))))((()))(()())" stackNew)

testeInvalido02::IO()
testeInvalido02 =
  assertEqual "Sintaxe Invalida" (contagem ")" stackNew)

testeValido02:: IO()
testeValido02 =
  assertEqual ("Sintaxe valida") (contagem "(2+3)+(5*(3-6/2))" stackNew)

testeInvalido::IO()
testeInvalido =
  assertEqual "Sintaxe Invalida" (contagem "(()" stackNew)



{-
)((()))()))())(( -> nao

()(()()) -> sim

()()()()()()()) -> nao

(())() -> sim

((((()))))((()))(()()) -> sim

((()))()()(())( - nao
-}
