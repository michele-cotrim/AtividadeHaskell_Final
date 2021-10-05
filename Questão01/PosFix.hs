{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{--Elaborar um programa em Haskell para solicitar ao usuário (E/S) uma expressão aritmética
(+,-,*,/) pós-fixa, avaliar a expressão e imprimir o resultado. A avaliação da expressão deverá ser
realizada utilizando um tipo abstrato de dados Pilha.--}
{- Temos uma pilha e vamos fazendo colocando os elementos da string de entrada, apos vamos desempilhando
e fazendo as operações quando necessário-}

module PoxFix where
import Stack
import Data.Char(ord, isDigit)
import Testes

{-
Função solveRPN recebe uma string (nossa formula pos fixa) e uma pilha
A função verifica se o topo da pilha é um digito e se for pega o numero ascii dele e diminui 48
Quando enconta um dos simbolos faz a operação com as duas proximas expressões 
-}


solveRPN :: String -> Stack String -> Stack String
solveRPN x stack = solveRPNAux(pegarDezena(x)) stack

solveRPNAux:: [String] -> Stack String -> Stack String
solveRPNAux [] stack = stack
solveRPNAux (x:s) stack
  |x == "+" = solveRPNAux s (push (show(exp2 + exp1)) stackNew)
  |x == "-" = solveRPNAux s (push (show(exp2 - exp1)) stackNew)
  |x == "*" = solveRPNAux s (push (show(exp2 * exp1)) stackNew)
  |x == "/" = solveRPNAux s (push (show(exp2 / exp1)) stackNew)
  |otherwise = solveRPNAux s (push x stack)
  where
    exp1 = read (peek stack ) :: Double
    exp2 = read (peek(pop stack)) :: Double
    stackNew = pop (pop stack)


pegarDezena :: String -> [String]
pegarDezena exp =  words (exp)


main :: IO ()
main = do
  --print (peek(solveRPN "100 200 + 2 / 5 * 7 +" stackNew))
  teste01
  teste02
  teste03


teste01::IO()
teste01 = assertEqual "757.0" (peek(solveRPN "100 200 + 2 / 5 * 7 +" stackNew))

teste02::IO()
teste02 = assertEqual "3.0" (peek(solveRPN "7 8 + 3 2 + /" stackNew))

teste03::IO()
teste03 = assertEqual "23.0" (peek(solveRPN "10 2 8 * + 3 -" stackNew))

