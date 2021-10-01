{- Elaborar um programa em Haskell para criar uma Agenda de Contatos (CRUD) com as
seguintes informações: CPF, Nome, Telefone, E-mail. A agenda deverá permitir a inclusãook ,
exclusão, alteração, listagem totalok e pesquisa dos contatos por CPF e por Nome. Utilizar na
resolução entrada/saída, módulos, tipos algébricos, arquivos.
-}

module Agenda where

import System.IO (putStrLn)
import Contatos
import OpcaoAdicionar
import OpcaoListar
import OpcaoExcluir



contatos :: [Contatos]
contatos = [Contatos "Cristiano" "12345678910" "20191xxxx@uesb.edu.br" "77981599288" , Contatos "Maria" "123.456.789.10" "maria@gmail.com" "77 999697720"]

agenda :: [Contatos] -> IO()
agenda contato = do 
    putStrLn ("\nInforme o número da ação desejada:" ++ "\n\n" ++ "1-Adicionar contato" ++ "\n2-Excluir contato" ++ "\n3-Editar contato" 
      ++ "\n4-Listar todos os contatos" ++ "\n5-Pesquisar contato" ++ "\nX-Sair")
    acao <- getLine
    case acao  of
      "1" -> do
            novosContatos <- adicionando contato
            agenda novosContatos
      "2" -> do 
            putStrLn ("1-Nome" ++ "\n2-CPF")
            opcao <- getLine
            escolha <- getLine
            case opcao of
              "1" -> do
                    contatoNovo <- excluirNome contato escolha
                    putStrLn ("")
                    agenda contatoNovo
                      
              "2" -> do
                     contatoNovo <- excluirCpf contato escolha
                     putStrLn ("")
                     agenda contatoNovo
      "3" -> do 
            putStrLn ("3")
            agenda contato
      "4" -> do 
            putStrLn $ imprimir contato
            agenda contato
      "5" -> do 
            putStrLn ("5")
            agenda contato
      "X" -> do putStrLn ("Obrigada por usar esse programa")

      "x" -> do putStrLn ("Obrigada por usar esse programa")
      
      _ -> do 
            putStrLn ("Informe uma opção valida")
            agenda contato
  





main :: IO ()
main = do 
      (agenda contatos)-- input eh lido como string
      
