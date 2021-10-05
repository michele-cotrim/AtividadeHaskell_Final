{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module OpcaoPesquisa where

import Contatos
{-Recebe o nome ou cpf para ser buscado lança para a função auxiliar e retorna para o main o que a auxiliar encontrou-}
pesquisaNome:: [Contatos] -> String -> String 
pesquisaNome contato nome = auxiliarNome contato nome

pesquisaCpf:: [Contatos] -> String -> String
pesquisaCpf contato cpf = auxiliarCpf contato cpf

{-Nas funções auxiliares a lista de contatos é percorrida e seus componentes separados, quando o contato é encontrado ele é retornado
Se não for encontrado retorna que não foi encontrado -}
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
