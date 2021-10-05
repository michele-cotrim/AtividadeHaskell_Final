module OpcaoAdicionar where
import Contatos
import OpcaoListar

{-MOdulo responsavel pela função de adicionar 
No adicionando é informado os dados do contato, gera um novo contato e no main esse contato é inserido no arquivo-}
adicionando:: [Contatos] -> IO[Contatos]
adicionando contato = do
                      putStrLn"\nInforme o nome"
                      nome <- getLine
                      putStrLn"Informe o cpf"
                      cpf <- getLine
                      putStrLn "Informe o email"
                      email <- getLine
                      putStrLn"Informe o telefone"
                      telefone <- getLine
                      putStrLn $ imprimir (contato ++ [Contatos nome cpf email telefone])
                      return (contato ++ [Contatos nome cpf email telefone])
{-Função usada pelo editar nome do contato-}
adicionarSimples::[Contatos] -> Contatos -> [Contatos]
adicionarSimples lista contato = lista ++ [contato]

{-Função usada pelo editar email, cpf, telefone-}
adicionarSimples2::[Contatos] -> Contatos -> IO[Contatos]
adicionarSimples2 lista contato = return $(lista ++ [contato])