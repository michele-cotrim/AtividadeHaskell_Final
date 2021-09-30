{--Elaborar um programa em Haskell para solicitar ao usuário (E/S) uma expressão aritmética
(+,-,*,/) pós-fixa, avaliar a expressão e imprimir o resultado. A avaliação da expressão deverá ser
realizada utilizando um tipo abstrato de dados Pilha.--}
{- Temos uma pilha e vamos fazendo colocando os elementos da string de entrada, apos vamos desempilhando
e fazendo as operações quando necessário-}

module PoxFix where
import Stack
import Data.Char(ord, isDigit)



solveRPN :: String -> Stack Int -> Stack Int
solveRPN [] stack = stack
solveRPN (x:xs) stack 
  |isDigit x = solveRPN xs (push (ord(x)-48) stack)
  |x == '+' = solveRPN xs (push (exp1 + exp2) stackNew)
  |x == '-' = solveRPN xs (push (exp1 - exp2) stackNew)
  |x == '*' = solveRPN xs (push (exp1 * exp2) stackNew)
  |x == '/' = solveRPN xs (push (div exp1 exp2) stackNew)
  |x == ' ' = solveRPN xs stack


  --x == '+' = solveRPN xs  (( push (char(exp1)+(exp2)+48 ))   stackNew )
  where 
    exp1 = (peek stack)
    exp2 = (peek(pop stack))
    stackNew = (pop (pop stack))



main :: IO ()
main = do putStrLn (show (peek(solveRPN "2 2 4 + *" stackNew)))
