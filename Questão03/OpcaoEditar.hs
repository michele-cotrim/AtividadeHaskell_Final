{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module OpcaoEditar where

import Contatos ( Contatos(..) )
import OpcaoPesquisa ()
import OpcaoExcluir ()
import OpcaoAdicionar ( adicionarSimples )
{-Editar nome retorna a lista de conatos atualizada para o IO da Agenda que vai ficar responsavel 
posteriormente por excluir o contato com nome antigo -}
editarNome:: [Contatos] -> String -> String -> IO[Contatos]
editarNome contato nomeAtual nome =
  return $ alterarNome contato (pesquisarNome contato nomeAtual) nome

{-Editar CPF, Email, Telefone retornam so o contato atalizado, não uma lista deles pois na hora de 
excluirmos por nome todos com nome igual serão retirados da lista-}
editarCpf:: [Contatos] -> String -> String -> IO Contatos
editarCpf contato nome cpf =
  return $ alterarCpf contato (pesquisarNome contato nome) cpf

editarEmail:: [Contatos] -> String -> String -> IO Contatos
editarEmail contato nome email =
  return $ alterarEmail contato (pesquisarNome contato nome) email

editarTelefone:: [Contatos] -> String -> String -> IO Contatos
editarTelefone contato nome telefone =
  return $ alterarTelefone contato (pesquisarNome contato nome) telefone

{-Nas funções alterar cada uma separa os componentes de contato e altera seu respectivo-}
alterarNome :: [Contatos] -> Contatos -> String -> [Contatos]
alterarNome contatos Vazio nome = []
alterarNome contatos (Contatos n c e t) nome =  adicionarSimples contatos (Contatos nome c e t)

alterarCpf :: [Contatos] -> Contatos -> String -> Contatos
alterarCpf contatos Vazio cpf = Vazio
alterarCpf contatos (Contatos n c e t) cpf = (Contatos n cpf e t)

alterarEmail :: [Contatos] -> Contatos -> String -> Contatos
alterarEmail contatos Vazio email = Vazio
alterarEmail contatos (Contatos n c e t) email = (Contatos n c email t)


alterarTelefone :: [Contatos] -> Contatos -> String -> Contatos
alterarTelefone contatos Vazio nome = Vazio
alterarTelefone contatos (Contatos n c e t) telefone = (Contatos n c e telefone)

pesquisarNome:: [Contatos] -> String -> Contatos
pesquisarNome [] nome = Vazio
pesquisarNome ((Contatos n c e t):xs) nome
  |n==nome = Contatos n c e t
  |otherwise = pesquisarNome xs nome