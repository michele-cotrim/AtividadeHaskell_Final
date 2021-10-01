module OpcaoExcluir where

import Contatos


excluirNome::[Contatos] -> String -> IO([Contatos])
excluirNome contato nome =  return(auxiliarNome contato nome)


excluirCpf::[Contatos] -> String -> IO([Contatos])
excluirCpf contato cpf = return(auxiliarCpf contato cpf)

auxiliarNome:: [Contatos] -> String -> [Contatos]
auxiliarNome [] nome = []
auxiliarNome ((Contatos n c e t):xs) nome
  |n==nome = auxiliarNome xs nome
  |otherwise = (Contatos n c e t):auxiliarNome xs nome

auxiliarCpf :: [Contatos] -> String -> [Contatos]
auxiliarCpf [] cpf = []
auxiliarCpf ((Contatos n c e t):xs) cpf
  |c==cpf = auxiliarNome xs cpf
  |otherwise = (Contatos n c e t):auxiliarCpf xs cpf