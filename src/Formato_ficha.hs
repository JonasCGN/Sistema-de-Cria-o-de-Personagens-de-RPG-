module Formato_ficha (
    salvarPersonagensFicha,
    carregarPersonagensFicha
) where

import Personagem
import Nomeavel 
import System.IO
import Control.Exception (IOException, catch)
import Data.List (intercalate, find, isPrefixOf, dropWhile)
import Text.Read (readMaybe)
import Data.Char (toLower, isSpace) 
import Data.Maybe (fromMaybe)  -- Adicionada a importação

-- Função auxiliar para remover espaços no início e fim (copiada do main)
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile Data.Char.isSpace

-- Função auxiliar para parsear Maybe (copiada do main)
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr go []
    where go x xs = case f x of
                      Just y -> y : xs
                      Nothing -> xs

-- Converte um Item para String formatada
itemParaFicha :: Item -> String
itemParaFicha (Item n d) = "  - " ++ n ++ ": " ++ d

-- Converte um Personagem para formato de ficha String
personagemParaFicha :: Personagem -> String
personagemParaFicha p = 
  unlines [
    "=======================================",
    "           FICHA DE PERSONAGEM",
    "=======================================",
    "Nome: " ++ obterNome p,
    "Classe: " ++ show (classe p),
    "Raça: " ++ show (raca p),
    "---------------------------------------",
    "Atributos:",
    "  Força:        " ++ show (forca $ atributos p),
    "  Inteligência: " ++ show (inteligencia $ atributos p),
    "  Destreza:     " ++ show (destreza $ atributos p),
    "---------------------------------------",
    "Inventário:",
    if null (inventario p)
      then "  (Vazio)"
      else intercalate "\n" (map itemParaFicha (inventario p)),
    "---------------------------------------",
    "História:",
    if null (historia p)
      then "  (Sem história)"
      else historia p,
    "======================================="
  ]

-- Salva a lista de personagens no formato de ficha
salvarPersonagensFicha :: FilePath -> [Personagem] -> IO ()
salvarPersonagensFicha path ps = writeFile path (intercalate "\n\n" (map personagemParaFicha ps)) -- Adiciona linha extra entre fichas

-- Função auxiliar para extrair valor de uma linha chave-valor
extrairValor :: String -> String -> Maybe String
extrairValor chave linha = 
  if chave `isPrefixOf` linha
  then Just $ trim $ drop (length chave) linha
  else Nothing

-- Converte String de volta para Classe (com tratamento de erro)
stringParaClasse :: String -> Maybe Classe
stringParaClasse s = case s of
  "Barbaro" -> Just Barbaro
  "Bardo" -> Just Bardo
  "Bruxo" -> Just Bruxo
  "Clerigo" -> Just Clerigo
  "Druida" -> Just Druida
  "Feiticeiro" -> Just Feiticeiro
  "Guerreiro" -> Just Guerreiro
  "Ladino" -> Just Ladino
  "Mago" -> Just Mago
  "Monge" -> Just Monge
  "Paladino" -> Just Paladino
  "Patrulheiro" -> Just Patrulheiro
  _ -> Nothing

-- Converte String de volta para Raça (com tratamento de erro)
stringParaRaca :: String -> Maybe Raca
stringParaRaca s = case s of
  "Humano" -> Just Humano
  "Elfo" -> Just Elfo
  "Anão" -> Just Anão
  "Halfling" -> Just Halfling
  "MeioOrc" -> Just MeioOrc
  "MeioElfo" -> Just MeioElfo
  "Gnomo" -> Just Gnomo
  "Draconato" -> Just Draconato
  "Tiefling" -> Just Tiefling
  _ -> Nothing

-- Parse de um item do inventário
parseItem :: String -> Maybe Item
parseItem linha = 
  let trimmed = trim $ dropWhile (== '-') $ trim linha -- Remove '  - ' inicial
      (nomeStr, descStr) = break (== ':') trimmed
  in if null descStr || null nomeStr
     then Nothing
     else Just $ Item (trim nomeStr) (trim $ drop 1 descStr)

-- Função parse de uma ficha (String) para Personagem
fichaParaPersonagem :: String -> Maybe Personagem
fichaParaPersonagem fichaStr = 
  let linhas = lines fichaStr
      getNome = extrairValor "Nome: " =<< find (isPrefixOf "Nome: ") linhas
      getClasseStr = extrairValor "Classe: " =<< find (isPrefixOf "Classe: ") linhas
      getRacaStr = extrairValor "Raça: " =<< find (isPrefixOf "Raça: ") linhas
      getForcaStr = extrairValor "Força:        " =<< find (isPrefixOf "  Força:        ") linhas
      getIntStr = extrairValor "Inteligência: " =<< find (isPrefixOf "  Inteligência: ") linhas
      getDexStr = extrairValor "Destreza:     " =<< find (isPrefixOf "  Destreza:     ") linhas
      getHistoriaStr = extrairValor "História: " =<< find (isPrefixOf "História: ") linhas
      
      -- Encontra a seção do inventário
      invSecao = dropWhile (not . isPrefixOf "Inventário:") linhas
      invLinhas = if null invSecao then [] else takeWhile (not . isPrefixOf "=======================================") (drop 1 invSecao)
      
      -- Parse dos itens, ignorando a linha '(Vazio)'
      parsedItens = mapMaybe parseItem $ filter (isPrefixOf "  - ") invLinhas

      -- Captura a história
      historia = getHistoriaStr -- Agora é só uma string simples

  in do
    -- Obtém e valida os valores extraídos
    nome <- getNome
    classeStr <- getClasseStr
    classe' <- stringParaClasse classeStr
    racaStr <- getRacaStr
    raca' <- stringParaRaca racaStr
    forcaStr <- getForcaStr
    forca' <- readMaybe forcaStr :: Maybe Int
    intStr <- getIntStr
    int' <- readMaybe intStr :: Maybe Int
    dexStr <- getDexStr
    dex' <- readMaybe dexStr :: Maybe Int

    -- Agora a história é apenas uma string
    let historia' = fromMaybe "(Sem história)" historia

    -- Aqui, garantimos que parsedItens está no formato correto
    return $ Personagem
      nome
      classe'
      raca'
      (Atributos forca' int' dex')
      parsedItens      -- Lista de Item
      historia'        -- História como uma string simples


-- Divide o conteúdo do arquivo em blocos de ficha (String)
splitFichas :: String -> [String]
splitFichas = go . lines
  where
    go [] = []
    go ls = 
      -- Encontra o início da próxima ficha
      let inicioFicha = dropWhile (not . isPrefixOf "=======================================") ls
      in if null inicioFicha 
         then []
         else 
           -- Pega a ficha até o próximo separador duplo
           let (fichaAtual, resto) = break (== "=======================================") (drop 1 inicioFicha)
               fichaCompleta = unlines (take (length fichaAtual + 1) inicioFicha) -- Inclui o separador final
           in fichaCompleta : go (drop 1 resto) -- Continua do próximo separador

-- Carrega a lista de personagens do formato de ficha
carregarPersonagensFicha :: FilePath -> IO (Either String [Personagem])
carregarPersonagensFicha path = 
  catch (do
          conteudo <- readFile path
          if null (trim conteudo) 
            then return $ Right [] -- Retorna lista vazia se o arquivo estiver vazio
            else do
              let fichasStr = splitFichas conteudo
              let maybePersonagens = mapMaybe fichaParaPersonagem fichasStr -- Tenta parsear todas as fichas
              if length maybePersonagens == length fichasStr
                then return $ Right maybePersonagens
                else return $ Left "Erro ao parsear uma ou mais fichas no arquivo. Verifique o formato."
        )
        (\e -> return $ Left $ "Erro ao ler o arquivo: " ++ show (e :: IOException))
