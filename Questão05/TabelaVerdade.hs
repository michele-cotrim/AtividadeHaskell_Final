{-Elaborar um programa em Haskell para construir a tabela verdade de uma fórmula
proposicional baseado no tipo abaixo. Defina também uma função que determina se uma fórmula é
ou não uma tautologia.-}

module TabelaVerdade where 

main::IO()
main = do putStrLn("Insira o que deseja gerar com base no exemplo abaixo:" 
            ++ "Var X\nNot Z\nAnd X Z\nOr X Z")
          l <- getLine
          putStrLn(l)
        
{-

-}