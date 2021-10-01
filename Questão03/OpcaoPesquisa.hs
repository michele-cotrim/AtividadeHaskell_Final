{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module OpcaoPesquisa where

import Contatos

pesquisaNome:: [Contatos] -> String -> String 
pesquisaNome contato nome = auxiliarNome contato nome

pesquisaCpf:: [Contatos] -> String -> String
pesquisaCpf contato cpf = auxiliarCpf contato cpf

auxiliarNome:: [Contatos] -> String -> String
auxiliarNome [] nome = "Não encontrado"
auxiliarNome ((Contatos n c e t):xs) nome
  |n==nome = ("Nome = " ++ n) ++ ("  | CPF = " ++ c) ++ ("  |  Email = " ++ e )++ ("  |  Telefone = " ++ t ) ++ "\n"
  |otherwise = auxiliarNome xs nome

auxiliarCpf :: [Contatos] -> String -> String
auxiliarCpf [] cpf = "Não encontrado"
auxiliarCpf ((Contatos n c e t):xs) cpf
  |c==cpf = ("Nome = " ++ n) ++ ("  | CPF = " ++ c) ++ ("  |  Email = " ++ e )++ ("  |  Telefone = " ++ t ) ++ "\n"
  |otherwise = auxiliarCpf xs cpf
