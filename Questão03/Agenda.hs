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

contatos :: [Contatos]
contatos = [Contatos "Cristiano" "12345678910" "20191xxxx@uesb.edu.br" "77981599288", Contatos "Maria" "123.456.789.10" "maria@gmail.com" "77 999697720"]

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
      agenda novosContatos
    "2" -> do
      putStrLn ("1-Nome" ++ "\n2-CPF")
      opcao <- getLine
      escolha <- getLine
      case opcao of
        "1" -> do
          contatoNovo <- excluirNome contato escolha
          putStrLn ""
          agenda contatoNovo
        "2" -> do
          contatoNovo <- excluirCpf contato escolha
          putStrLn ""
          agenda contatoNovo
    "3" -> do
      putStrLn "Insira o nome do contato"
      nome <- getLine
      putStrLn(pesquisaNome contato nome) --verificar dps se o contato existe
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
                              putStrLn $ "\nContato auxiliar\n" ++ imprimir contatoAux
                              contatoNovo <- excluirNome contatoAux nome
                              putStrLn $ "\nContato novo\n" ++ imprimir contatoNovo
                              agenda contatoNovo
                        "2" -> do -- gerar contato novo -- excluir o antigo -- adicionar o novo
                              contatoAux <- editarCpf contato nome valor -- ctt novo
                              putStrLn $ "\nContato auxiliar\n" ++ imprimirContato contatoAux
                              ctt <- excluirNome contato nome
                              contatoNovo <- adicionarSimples2 ctt contatoAux 
                              putStrLn $ "\nContato novo\n" ++ imprimir contatoNovo
                              agenda contatoNovo
                              
                        "3" -> do
                              contatoAux <- editarEmail contato nome valor -- ctt novo
                              putStrLn $ "\nContato auxiliar\n" ++ imprimirContato contatoAux
                              ctt <- excluirNome contato nome
                              contatoNovo <- adicionarSimples2 ctt contatoAux 
                              putStrLn $ "\nContato novo\n" ++ imprimir contatoNovo
                              agenda contatoNovo

                        "4" -> do
                              contatoAux <- editarTelefone contato nome valor -- ctt novo
                              putStrLn $ "\nContato auxiliar\n" ++ imprimirContato contatoAux
                              ctt <- excluirNome contato nome
                              contatoNovo <- adicionarSimples2 ctt contatoAux 
                              putStrLn $ "\nContato novo\n" ++ imprimir contatoNovo
                              agenda contatoNovo

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

main :: IO ()
main =
  (agenda contatos) -- input eh lido como string
