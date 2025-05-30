-- Sistema de Criação de Personagens de RPG
-- Tema: Criação de personagens de RPG
-- Justificativa: Permite aplicar conceitos de programação funcional, como tipos algébricos, listas, abstrações, ADT, modularização e IO, de forma criativa e divertida.

module Main where

import Personagem
import Nomeavel
import Aleatorio
import System.IO
import Data.Char (toUpper, toLower, isSpace)
import qualified Data.Map as Map
import System.Info (os)
import System.Process (callCommand)

-- Funções para cores e estilos ANSI
corReset :: String
corReset = "\ESC[0m"

corVermelha :: String -> String
corVermelha s = "\ESC[31m" ++ s ++ corReset

corVerde :: String -> String
corVerde s = "\ESC[32m" ++ s ++ corReset

corAmarela :: String -> String
corAmarela s = "\ESC[33m" ++ s ++ corReset

corAzul :: String -> String
corAzul s = "\ESC[34m" ++ s ++ corReset

corMagenta :: String -> String
corMagenta s = "\ESC[35m" ++ s ++ corReset

corCiano :: String -> String
corCiano s = "\ESC[36m" ++ s ++ corReset

textoNegrito :: String -> String
textoNegrito s = "\ESC[1m" ++ s ++ corReset

textoSublinhado :: String -> String
textoSublinhado s = "\ESC[4m" ++ s ++ corReset

-- Combinações de estilo
tituloMenu :: String -> String
tituloMenu = textoNegrito . corAmarela

opcaoNumero :: String -> String
opcaoNumero = textoNegrito . corCiano

promptUsuario :: String -> String
promptUsuario = corVerde

mensagemErro :: String -> String
mensagemErro = corVermelha

mensagemSucesso :: String -> String
mensagemSucesso = corVerde

cabecalhoSecao :: String -> String
cabecalhoSecao = textoSublinhado . corAzul

descricaoPersonagem :: String -> String
descricaoPersonagem = corMagenta

-- Função para limpar o terminal
limparTerminal :: IO ()
limparTerminal = 
  if os == "mingw32"  -- Windows
  then callCommand "cls"
  else callCommand "clear"  -- Para sistemas Unix-like

type Contadores = Map.Map Int Int

-- Função para obter o próximo contador para uma seed
proximoContador :: Int -> Contadores -> (Int, Contadores)
proximoContador seed contadores =
  case Map.lookup seed contadores of
    Nothing -> (0, Map.insert seed 1 contadores)
    Just n  -> (n, Map.insert seed (n + 1) contadores)

-- Função que recebe a seed e contador e gera personagem
gerarPersonagemComContador :: Int -> Int -> Personagem
gerarPersonagemComContador seed contador = gerarPersonagemAleatorio (seed + contador)

-- Função auxiliar para remover espaços no início e fim
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Mostrar personagens com estilo
mostrarPersonagens :: [Personagem] -> IO ()
mostrarPersonagens [] = putStrLn $ "\n" ++ mensagemErro "Nenhum personagem cadastrado ainda."
mostrarPersonagens ps = do
    putStrLn $ "\n" ++ cabecalhoSecao "Personagens Cadastrados:"
    mapM_ (putStrLn . descricaoPersonagem . descricaoCompleta) ps

-- Adicionar item a personagem
adicionarItemPersonagem :: String -> String -> String -> [Personagem] -> [Personagem]
adicionarItemPersonagem nomeItem nomePersonagem descricao ps =
  let (antes, resto) = break (\p -> obterNome p == nomePersonagem) ps
  in case resto of
       [] -> ps  -- Personagem não encontrado
       (p:depois) ->
         let pAtualizado = adicionarItem (Item nomeItem descricao) p
         in antes ++ [pAtualizado] ++ depois

-- Adicionar personagem
adicionarPersonagem :: Personagem -> [Personagem] -> [Personagem]
adicionarPersonagem p ps = p : ps

-- Remover personagem
removerPersonagemMain :: String -> [Personagem] -> [Personagem]
removerPersonagemMain nome ps = filter (\p -> obterNome p /= nome) ps

-- Função principal do menu com contador e estilo
menuComContador :: [Personagem] -> Contadores -> IO ()
menuComContador ps contadores = do
  putStrLn $ "\n" ++ tituloMenu "==============================="
  putStrLn $ "    " ++ tituloMenu "Menu do Sistema de RPG"
  putStrLn $ tituloMenu "==============================="
  putStrLn $ (opcaoNumero "1.") ++ " Listar personagens"
  putStrLn $ (opcaoNumero "2.") ++ " Adicionar personagem"
  putStrLn $ (opcaoNumero "3.") ++ " Remover personagem"
  putStrLn $ (opcaoNumero "4.") ++ " Adicionar item a personagem"
  putStrLn $ (opcaoNumero "5.") ++ " Remover item de personagem"
  putStrLn $ (opcaoNumero "6.") ++ " Salvar personagens em arquivo"
  putStrLn $ (opcaoNumero "7.") ++ " Carregar personagens do arquivo"
  putStrLn $ (opcaoNumero "8.") ++ " Gerar personagem aleatório"
  putStrLn $ (opcaoNumero "0.") ++ " Sair"
  putStr $ promptUsuario "Escolha uma opção: "
  hFlush stdout
  op <- getLine
  case op of
    "1" -> do
      limparTerminal
      mostrarPersonagens ps
      menuComContador ps contadores

    "2" -> do
      limparTerminal
      putStrLn $ corAmarela "Caso você escreva um valor inválido nas opções abaixo, seu personagem receberá as configurações padrão (Guerreiro/Humano)"
      putStr $ promptUsuario "Nome: "; hFlush stdout; n <- getLine
      putStrLn $ textoSublinhado "\nEscolha a Classe:"
      putStrLn $ (opcaoNumero "0.") ++ " Barbaro"
      putStrLn $ (opcaoNumero "1.") ++ " Bardo"
      putStrLn $ (opcaoNumero "2.") ++ " Bruxo"
      putStrLn $ (opcaoNumero "3.") ++ " Clerigo"
      putStrLn $ (opcaoNumero "4.") ++ " Druida"
      putStrLn $ (opcaoNumero "5.") ++ " Feiticeiro"
      putStrLn $ (opcaoNumero "6.") ++ " Guerreiro"
      putStrLn $ (opcaoNumero "7.") ++ " Ladino"
      putStrLn $ (opcaoNumero "8.") ++ " Mago"
      putStrLn $ (opcaoNumero "9.") ++ " Monge"
      putStrLn $ (opcaoNumero "10.") ++ " Paladino"
      putStrLn $ (opcaoNumero "11.") ++ " Patrulheiro"
      putStr $ promptUsuario "Digite o número da classe: "; hFlush stdout; cIdx <- readLn
      let classe' = case cIdx of
                      0 -> Barbaro
                      1 -> Bardo
                      2 -> Bruxo
                      3 -> Clerigo
                      4 -> Druida
                      5 -> Feiticeiro
                      6 -> Guerreiro
                      7 -> Ladino
                      8 -> Mago
                      9 -> Monge
                      10 -> Paladino
                      11 -> Patrulheiro
                      _ -> Guerreiro -- padrão

      putStrLn $ textoSublinhado "\nEscolha a Raça:"
      putStrLn $ (opcaoNumero "0.") ++ " Humano"
      putStrLn $ (opcaoNumero "1.") ++ " Elfo"
      putStrLn $ (opcaoNumero "2.") ++ " Anão"
      putStrLn $ (opcaoNumero "3.") ++ " Halfling"
      putStrLn $ (opcaoNumero "4.") ++ " Meio-Orc"
      putStrLn $ (opcaoNumero "5.") ++ " Meio-Elfo"
      putStrLn $ (opcaoNumero "6.") ++ " Gnomo"
      putStrLn $ (opcaoNumero "7.") ++ " Draconato"
      putStrLn $ (opcaoNumero "8.") ++ " Tiefling"
      putStr $ promptUsuario "Digite o número da raça: "; hFlush stdout; rIdx <- readLn
      let raca' = case rIdx of
                    0 -> Humano
                    1 -> Elfo
                    2 -> Anão
                    3 -> Halfling
                    4 -> MeioOrc
                    5 -> MeioElfo
                    6 -> Gnomo
                    7 -> Draconato
                    8 -> Tiefling
                    _ -> Humano -- padrão

      putStr $ promptUsuario "Força: "; hFlush stdout; f <- readLn
      putStr $ promptUsuario "Inteligência: "; hFlush stdout; i <- readLn
      putStr $ promptUsuario "Destreza: "; hFlush stdout; d <- readLn
      let novo = criarPersonagem n classe' raca' (Atributos f i d)
      putStrLn $ "\n" ++ mensagemSucesso ("Personagem '" ++ n ++ "' criado com sucesso!")
      menuComContador (adicionarPersonagem novo ps) contadores

    "3" -> do
      limparTerminal
      putStr $ promptUsuario "Nome do personagem a remover: "; hFlush stdout; n <- getLine
      let nomeNormalizado = map toLower (trim n)
      if any (\p -> map toLower (trim (obterNome p)) == nomeNormalizado) ps
        then do
          let ps' = removerPersonagemMain n ps
          putStrLn $ mensagemSucesso "Personagem removido com sucesso!"
          menuComContador ps' contadores
        else do
          putStrLn $ mensagemErro "Erro: Personagem não encontrado!"
          menuComContador ps contadores

    "4" -> do
      limparTerminal
      putStr $ promptUsuario "Nome do personagem: "; hFlush stdout; n <- getLine
      putStr $ promptUsuario "Nome do item: "; hFlush stdout; ni <- getLine
      putStr $ promptUsuario "Descrição do item: "; hFlush stdout; di <- getLine
      let nomeNormalizado = map toLower (trim n)
      -- Verifica se o personagem existe usando nome normalizado
      if any (\p -> map toLower (trim (obterNome p)) == nomeNormalizado) ps
        then do
          -- Chama a função corrigida que também usa nome normalizado internamente
          let ps' = adicionarItemPersonagem ni n di ps 
          putStrLn $ mensagemSucesso "Item adicionado com sucesso!"
          menuComContador ps' contadores
        else do
          putStrLn $ mensagemErro "Erro: Personagem não encontrado!"
          menuComContador ps contadores

    "5" -> do
      limparTerminal
      putStr $ promptUsuario "Nome do personagem: "; hFlush stdout; n <- getLine
      putStr $ promptUsuario "Nome do item a remover: "; hFlush stdout; ni <- getLine
      let nomeNormalizado = map toLower (trim n)

      case filter (\p -> map toLower (trim (obterNome p)) == nomeNormalizado) ps of
        [] -> do
          putStrLn $ mensagemErro "Erro: Personagem não encontrado!"
          menuComContador ps contadores
        (pEncontrado:_) ->
          let personagemAtualizado = removerItem ni pEncontrado
          in if personagemAtualizado == pEncontrado
              then do
                putStrLn $ mensagemErro "Erro: Item não encontrado no personagem!"
                menuComContador ps contadores
              else do
                let ps' = map (\p -> if map toLower (trim (obterNome p)) == nomeNormalizado
                                      then personagemAtualizado
                                      else p) ps
                putStrLn $ mensagemSucesso "Item removido com sucesso!"
                menuComContador ps' contadores

    "6" -> do
      limparTerminal
      writeFile "personagens.txt" (show ps)
      putStrLn $ "\n" ++ mensagemSucesso "Personagens salvos com sucesso!"
      menuComContador ps contadores

    "7" -> do
      limparTerminal
      conteudo <- readFile "personagens.txt"
      let ps' = read conteudo :: [Personagem]
      putStrLn $ "\n" ++ mensagemSucesso "Personagens carregados com sucesso!"
      menuComContador ps' contadores

    "8" -> do
      limparTerminal
      putStrLn $ promptUsuario "Digite um número para gerar personagem aleatório (seed): "
      hFlush stdout
      seedStr <- getLine
      let seed = case reads seedStr of
                  [(n, _)] -> n
                  _        -> 12345 -- padrão se inválida
          (contadorAtual, contadoresAtualizados) = proximoContador seed contadores
          novo = gerarPersonagemComContador seed contadorAtual
      putStrLn $ "\n" ++ cabecalhoSecao "Personagem aleatório gerado:"
      putStrLn $ descricaoPersonagem (descricaoCompleta novo)
      menuComContador (adicionarPersonagem novo ps) contadoresAtualizados

    "0" -> putStrLn $ corAmarela "Saindo... Até logo!"

    _ -> do
      putStrLn $ mensagemErro "Opção inválida"
      menuComContador ps contadores

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

main :: IO ()
main = do
  putStrLn $ textoNegrito $ corCiano "Bem-vindo ao Sistema de Criação de Personagens de RPG!"
  menuComContador [] Map.empty

