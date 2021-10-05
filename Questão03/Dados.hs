{-Modulo responsavel pela leitura e escrita de dados do script-}
module Dados where

import Contatos

{-Ler o arquivo e gera a lista de contatos-}
ler::IO([Contatos])
ler = do 
      l <- readFile "ListaContatos.txt" 
      --lines l
      putStrLn $ show (lines l)
      return $ gerarContato (lines l)
{-Coloca contos e edições no arquivo-}
escrever::[Contatos] -> IO()
escrever contato = do 
            writeFile "ListaContatos.txt" (botaNoArquivo contato)

{-Formata o os contatos retirando os ;-}
formatar::String -> String -> [String]
formatar [] var = [var]
formatar (x:xs) var
  |x== ';' =  var :formatar xs "" 
  |otherwise = formatar xs (var ++ [x])

{-Separa as linhas do texto em contatos-}
gerarContato:: [String] -> [Contatos]
gerarContato lista = map (\var -> Contatos (formatar var "" !! 0) (formatar var "" !! 1) (formatar var "" !! 2) (formatar var "" !! 3)) lista

{-Formata inserção de contatos no arquivo-}
botaNoArquivo::[Contatos] -> String
botaNoArquivo [] = ""
botaNoArquivo (x:xs) =  auxilioBota x ++ "\n" ++ botaNoArquivo xs
{-Separa os componentes dos contatos e os formata com ; para serem separados no arquivo-}
auxilioBota:: Contatos -> String
auxilioBota (Contatos n c e t) = n++";"++ c ++";"++ e ++ ";" ++ t 