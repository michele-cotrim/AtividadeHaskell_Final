module OpcaoAdicionar where
import Contatos
import OpcaoListar

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

adicionarSimples::[Contatos] -> Contatos -> [Contatos]
adicionarSimples lista contato = lista ++ [contato]

adicionarSimples2::[Contatos] -> Contatos -> IO[Contatos]
adicionarSimples2 lista contato = return $(lista ++ [contato])