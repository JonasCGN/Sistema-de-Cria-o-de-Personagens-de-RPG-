module Formato_ficha (
    salvarPersonagensFicha,
    carregarPersonagensFicha
) where

import Personagem
import Nomeavel 
import System.IO
import Control.Exception (try, catch, IOException)
import Data.List (intercalate, find, isPrefixOf, dropWhile)
import Text.Read (readMaybe)
import Data.Char (toLower, isSpace) 
import Data.Maybe (catMaybes, mapMaybe,isJust)
import Data.Either (partitionEithers)
import qualified Data.Maybe as DM




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
-- Separadores usados (39 caracteres)
separadorIgual :: String
separadorIgual = replicate 39 '='

separadorTraco :: String
separadorTraco = replicate 39 '-'

-- Converte um Personagem para formato de ficha String com formato exato desejado
personagemParaFicha :: Personagem -> String
personagemParaFicha p = 
  unlines [
    separadorIgual,
    "Nome: " ++ obterNome p,
    "Classe: " ++ show (classe p),
    "Raça: " ++ show (raca p),
    separadorTraco,
    "Atributos:",
    "  Força:        " ++ show (forca $ atributos p),
    "  Inteligência: " ++ show (inteligencia $ atributos p),
    "  Destreza:     " ++ show (destreza $ atributos p),
    separadorTraco,
    "Inventário:",
    if null (inventario p)
      then "  (Vazio)"
      else intercalate "\n" (map itemParaFicha (inventario p)),
    separadorTraco,
    "História:",
    if null (historia p)
      then "  (Sem história)"
      else historia p,
    separadorIgual
  ]


-- Remove a última quebra de linha, se existir
removeUltimaQuebra :: String -> String
removeUltimaQuebra s = if not (null s) && last s == '\n' then init s else s


-- Salva a lista de personagens no formato de ficha
salvarPersonagensFicha :: FilePath -> [Personagem] -> IO ()
salvarPersonagensFicha path ps = writeFile path (intercalate "\n\n" (map (removeUltimaQuebra . personagemParaFicha) ps))



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
      getNome = extrairValor "Nome:" =<< find (isPrefixOf "Nome: ") linhas
      getClasseStr = extrairValor "Classe: " =<< find (isPrefixOf "Classe: ") linhas
      getRacaStr = extrairValor "Raça:" =<< find (isPrefixOf "Raça: ") linhas
      getForcaStr = extrairValor "Força:" =<< find (isPrefixOf "  Força:") linhas
      getIntStr = extrairValor "Inteligência:" =<< find (isPrefixOf "  Inteligência:") linhas
      getDexStr = extrairValor "Destreza:" =<< find (isPrefixOf "  Destreza:") linhas
      getHistoriaStr = extrairValor "História:" =<< find (isPrefixOf "História: ") linhas

      invSecao = dropWhile (not . isPrefixOf "Inventário:") linhas
      invLinhas = if null invSecao then [] else takeWhile (not . isPrefixOf "=======================================") (drop 1 invSecao)
      parsedItens = DM.mapMaybe parseItem $ filter (isPrefixOf "  - ") invLinhas

  in do
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
    let historia' = case getHistoriaStr of
                      Just h | h /= "(Sem história)" -> h
                      _ -> ""
    return $ Personagem
      nome
      classe'
      raca'
      (Atributos forca' int' dex')
      parsedItens
      historia'



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
carregarPersonagensFicha path = do
    resultado <- try (readFile path) :: IO (Either IOException String)
    case resultado of
      Left e -> return $ Left $ "Erro ao ler o arquivo: " ++ show e
      Right conteudo
        | null (trim conteudo) -> return $ Right []
        | otherwise -> do
            let fichasStr = splitFichas conteudo
            putStrLn $ "Fichas extraídas: " ++ show (length fichasStr)
            mapM_ (putStrLn . ("Ficha: " ++)) fichasStr

            let resultados = map fichaParaPersonagem fichasStr  -- [Maybe Personagem]
            putStrLn $ "Resultados do parse: " ++ show (map isJust resultados)

            let personagens = catMaybes resultados
            let errosCount = length (filter (== Nothing) resultados)

            if errosCount == 0
              then return $ Right personagens
              else return $ Left $ "Erros ao parsear " ++ show errosCount ++ " fichas"




