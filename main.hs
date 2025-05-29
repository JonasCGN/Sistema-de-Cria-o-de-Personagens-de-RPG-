-- Sistema de Criação de Personagens de RPG
-- Tema: Criação de personagens de RPG
-- Justificativa: Permite aplicar conceitos de programação funcional, como tipos algébricos, listas, abstrações, ADT, modularização e IO, de forma criativa e divertida.

module Main where

import Personagem
import Nomeavel
import System.IO

-- Função para mostrar todos os personagens com descrição completa
mostrarPersonagens :: [Personagem] -> IO ()
mostrarPersonagens ps = mapM_ (putStrLn . descricaoCompleta) ps

-- Função para adicionar personagem
adicionarPersonagem :: Personagem -> [Personagem] -> [Personagem]
adicionarPersonagem p ps = p : ps

-- Função para interação simples via terminal
menu :: [Personagem] -> IO ()
menu ps = do
    putStrLn "\nMenu do Sistema de RPG:"
    putStrLn "1. Listar personagens"
    putStrLn "2. Adicionar personagem"
    putStrLn "3. Remover personagem"
    putStrLn "4. Adicionar item a personagem"
    putStrLn "5. Remover item de personagem"
    putStrLn "6. Salvar personagens em arquivo"
    putStrLn "7. Carregar personagens do arquivo"
    putStrLn "0. Sair"
    putStr "Escolha: "
    hFlush stdout
    op <- getLine
    case op of
      "1" -> do mostrarPersonagens ps; menu ps
      "2" -> do
        putStr "Nome: "; hFlush stdout; n <- getLine
        putStr "Classe (Guerreiro/Mago/Ladino/Clerigo/Arqueiro): "; hFlush stdout; c <- getLine
        putStr "Raça (Humano/Elfo/Anao/Orc/Goblin): "; hFlush stdout; r <- getLine
        putStr "Força: "; hFlush stdout; f <- readLn
        putStr "Inteligência: "; hFlush stdout; i <- readLn
        putStr "Destreza: "; hFlush stdout; d <- readLn
        let classe' = read (capitalize c) :: Classe
            raca' = read (capitalize r) :: Raca
            novo = criarPersonagem n classe' raca' (Atributos f i d)
        menu (adicionarPersonagem novo ps)
      "3" -> do
        putStr "Nome do personagem a remover: "; hFlush stdout; n <- getLine
        menu (removerPersonagem n ps)
      "4" -> do
        putStr "Nome do personagem: "; hFlush stdout; n <- getLine
        putStr "Nome do item: "; hFlush stdout; ni <- getLine
        putStr "Descrição do item: "; hFlush stdout; di <- getLine
        let ps' = [if nome p == n then adicionarItem (Item ni di) p else p | p <- ps]
        menu ps'
      "5" -> do
        putStr "Nome do personagem: "; hFlush stdout; n <- getLine
        putStr "Nome do item a remover: "; hFlush stdout; ni <- getLine
        let ps' = [if nome p == n then removerItem ni p else p | p <- ps]
        menu ps'
      "6" -> do
        writeFile "personagens.txt" (show ps)
        putStrLn "Salvo com sucesso!"
        menu ps
      "7" -> do
        conteudo <- readFile "personagens.txt"
        let ps' = read conteudo :: [Personagem]
        putStrLn "Carregado com sucesso!"
        menu ps'
      "0" -> putStrLn "Saindo..."
      _   -> menu ps

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toEnum (fromEnum x - 32 * fromEnum (x >= 'a' && x <= 'z')) : xs

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Criação de Personagens de RPG!"
  menu []