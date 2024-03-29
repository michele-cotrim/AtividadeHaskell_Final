{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- Elaborar um programa em Haskell para criar uma Agenda de Contatos (CRUD) com as
seguintes informações: CPF, Nome, Telefone, E-mail. A agenda deverá permitir a inclusãook ,
exclusão, alteração, listagem totalok e pesquisa dos contatos por CPF e por Nome. Utilizar na
resolução entrada/saída, módulos, tipos algébricos, arquivos.
-}

module Agenda where

import Contatos
import OpcaoAdicionar ( adicionando, adicionarSimples, adicionarSimples2 )
import OpcaoExcluir ( excluirCpf, excluirNome, auxiliarCpf)
import OpcaoListar 
import OpcaoPesquisa
import System.IO (putStrLn)
import OpcaoEditar
import Dados

contatos :: [Contatos]
contatos = [Contatos "Cristiano" "12345678910" "20191xxxx@uesb.edu.br" "77981599288", Contatos "Maria" "123.456.789.10" "maria@gmail.com" "77 999697720"]


{-A funcao agenda é responsavel por todas as ações do script-}
agenda :: [Contatos] -> IO ()
agenda contato = do
  putStrLn
    ( "\n\nInforme o número da ação desejada:" ++ "\n\n" ++ "1-Adicionar contato" ++ "\n2-Excluir contato" ++ "\n3-Editar contato"
        ++ "\n4-Listar todos os contatos"
        ++ "\n5-Pesquisar contato"
        ++ "\nX-Sair"
    )
  acao <- getLine
  case acao of
    "1" -> do
      novosContatos <- adicionando contato
      escrever novosContatos
      agenda novosContatos
    "2" -> do
      putStrLn ("1-Nome" ++ "\n2-CPF")
      opcao <- getLine
      escolha <- getLine
      case opcao of
        "1" -> do
          contatoNovo <- excluirNome contato escolha
          putStrLn ""
          escrever contatoNovo
          agenda contatoNovo
        "2" -> do
          contatoNovo <- excluirCpf contato escolha
          putStrLn ""
          escrever contatoNovo
          agenda contatoNovo
    "3" -> do
          funcEditar contato

    "4" -> do
      putStrLn $ imprimir contato
      agenda contato
    "5" -> do
      putStrLn ("1-Nome" ++ "\n2-CPF")
      opcao <- getLine
      escolha <- getLine
      case opcao of
        "1" ->
          putStrLn (pesquisaNome contato escolha)
        "2" ->
          putStrLn (pesquisaCpf contato escolha)

      agenda contato
    "X" -> putStrLn ("Obrigada por usar esse programa")
    "x" -> putStrLn ("Obrigada por usar esse programa")
    _ -> do
      putStrLn "Informe uma opção valida"
      agenda contato


{-Trecho que tem a funcao editar para que nao fique muito extendo a parte de cima
Nessa funcao o usuario informa o nome do contato e confirma se realmente é o usuario desejado
Quando o contato é editado é gerado uma cópia dele e adicionado no fim da lista e o antigo excluido-}
funcEditar::[Contatos] -> IO()
funcEditar contato = do
            putStrLn "Insira o nome do contato"
            nome <- getLine
            putStrLn(pesquisaNome contato nome)
            putStrLn("É o contato que deseja editar?" ++ "\n1-Sim" ++ "\n2-Não")
            escolha <- getLine
            case escolha of
              "1" -> do
                    putStrLn"O que deseja editar? \n1-Nome\n2-Cpf\n3-Email\n4-Telefone"
                    alternativa <- getLine
                    valor <- getLine
                    case alternativa of
                          "1" -> do
                                contatoAux <- editarNome contato nome valor
                                contatoNovo <- excluirNome contatoAux nome
                                putStrLn $ "\nLista com contato alterado\n" ++ imprimir contatoNovo
                                escrever contatoNovo
                                agenda contatoNovo
                          "2" -> do -- gerar contato novo -- excluir o antigo -- adicionar o novo
                                contatoAux <- editarCpf contato nome valor -- ctt novo
                                ctt <- excluirNome contato nome -- exclui o contato antigo
                                contatoNovo <- adicionarSimples2 ctt contatoAux -- adiciona o contato novo na lista 
                                putStrLn $ "\nLista com contato alterado\n" ++ imprimir contatoNovo
                                escrever contatoNovo -- escreve o arquivo
                                agenda contatoNovo -- retorna pro menu
                                
                          "3" -> do
                                contatoAux <- editarEmail contato nome valor -- ctt novo
                                ctt <- excluirNome contato nome
                                contatoNovo <- adicionarSimples2 ctt contatoAux 
                                putStrLn $ "\nLista com contato alterado\n" ++ imprimir contatoNovo
                                escrever contatoNovo
                                agenda contatoNovo

                          "4" -> do
                                contatoAux <- editarTelefone contato nome valor -- ctt novo
                                ctt <- excluirNome contato nome
                                contatoNovo <- adicionarSimples2 ctt contatoAux 
                                putStrLn $ "\nLista com contato alterado\n" ++ imprimir contatoNovo
                                escrever contatoNovo
                                agenda contatoNovo
                          _ -> do 
                                funcEditar contato
              "2" -> do funcEditar contato                  


main :: IO ()
main = do
  declarativa <- ler
  (agenda declarativa) -- input eh lido como string
