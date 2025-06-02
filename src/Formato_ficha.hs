module Formato_ficha (
    salvarPersonagensFicha,
    carregarPersonagensFicha
) where

import Personagem
import Nomeavel
import System.IO
import Control.Exception (try, catch, IOException)
import Data.List (intercalate, find, findIndex, isPrefixOf, dropWhile, isInfixOf)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.Maybe (catMaybes, mapMaybe, isJust)
import Data.Either (partitionEithers)

-- Marcadores para o novo formato
marcadorInicioPersonagem :: String
marcadorInicioPersonagem = "[PERSONAGEM]"

marcadorFimPersonagem :: String
marcadorFimPersonagem = "[FIM_PERSONAGEM]"

marcadorInicioInventario :: String
marcadorInicioInventario = "[INVENTARIO]"

marcadorFimInventario :: String
marcadorFimInventario = "[FIM_INVENTARIO]"

marcadorInicioHistoria :: String
marcadorInicioHistoria = "[HISTORIA]"

marcadorFimHistoria :: String
marcadorFimHistoria = "[FIM_HISTORIA]"

-- Função auxiliar para remover espaços no início e fim
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile Data.Char.isSpace

-- Converte um Item para String formatada no novo formato
itemParaString :: Item -> String
itemParaString (Item n d) = n ++ ": " ++ d

-- Converte String de volta para Item (Corrigido novamente)
stringParaItem :: String -> Maybe Item
stringParaItem linha = 
  let (nomeStr, descRest) = break (== ':') linha -- Usa Char ':' para dividir a String
  -- Verifica se descRest não é vazio E se o primeiro caractere é de fato ':'
  in if null descRest || head descRest /= ':' 
     then Nothing -- ':' não encontrado ou não é o primeiro char de descRest
     -- drop 1 remove o ':' inicial de descRest antes de trimar
     else Just $ Item (trim nomeStr) (trim $ drop 1 descRest) 

-- Converte um Personagem para formato de String no novo formato
personagemParaString :: Personagem -> String
personagemParaString p = 
  unlines $ 
    [ marcadorInicioPersonagem
    , "Nome: " ++ obterNome p
    , "Classe: " ++ show (classe p)
    , "Raça: " ++ show (raca p)
    , "Forca: " ++ show (forca $ atributos p)
    , "Inteligencia: " ++ show (inteligencia $ atributos p)
    , "Destreza: " ++ show (destreza $ atributos p)
    , marcadorInicioInventario
    ] ++
    (if null (inventario p) then [] else map itemParaString (inventario p)) ++
    [ marcadorFimInventario
    , marcadorInicioHistoria
    ] ++
    (if null (historia p) then [] else lines (historia p)) ++
    [ marcadorFimHistoria
    , marcadorFimPersonagem
    ]

-- Salva a lista de personagens no novo formato
salvarPersonagensFicha :: FilePath -> [Personagem] -> IO ()
salvarPersonagensFicha path ps = 
    writeFile path (intercalate "\n" (map personagemParaString ps))

-- Função auxiliar para extrair valor de uma linha chave-valor
extrairValor :: String -> [String] -> Maybe String
extrairValor chave linhas = 
    -- Garante que a chave tenha um espaço após ela para evitar matches parciais (ex: "Forca:" vs "Forca Extra:")
    fmap (trim . drop (length chave)) (find (isPrefixOf chave) linhas)

-- Converte String de volta para Classe (sem Enum)
stringParaClasse :: String -> Maybe Classe
stringParaClasse s = lookup s classeMap
  where classeMap = [
          ("Barbaro", Barbaro), ("Bardo", Bardo), ("Bruxo", Bruxo),
          ("Clerigo", Clerigo), ("Druida", Druida), ("Feiticeiro", Feiticeiro),
          ("Guerreiro", Guerreiro), ("Ladino", Ladino), ("Mago", Mago),
          ("Monge", Monge), ("Paladino", Paladino), ("Patrulheiro", Patrulheiro)]

-- Converte String de volta para Raça (sem Enum, sintaxe revisada)
stringParaRaca :: String -> Maybe Raca
stringParaRaca s = lookup s racaMap
  where racaMap = [
          ("Humano", Humano), ("Elfo", Elfo), ("Anão", Anão),
          ("Halfling", Halfling), ("MeioOrc", MeioOrc), ("MeioElfo", MeioElfo),
          ("Gnomo", Gnomo), ("Draconato", Draconato), ("Tiefling", Tiefling)] -- Fechamento do colchete estava correto, mas revisado.

-- Extrai uma seção delimitada por marcadores de início e fim
extrairSecao :: String -> String -> [String] -> [String]
extrairSecao inicio fim linhas = 
    case findIndex (== inicio) linhas of
        Nothing -> []
        Just startIdx -> 
            case findIndex (== fim) (drop (startIdx + 1) linhas) of
                Nothing -> [] -- Marcador de fim não encontrado
                Just endOffset -> take endOffset (drop (startIdx + 1) linhas)

-- Função parse de um bloco de linhas (String) para Personagem
blocoParaPersonagem :: [String] -> Maybe Personagem
blocoParaPersonagem bloco = do
    -- Verifica marcadores de início e fim do bloco
    guard (not (null bloco) && head bloco == marcadorInicioPersonagem && last bloco == marcadorFimPersonagem)
    let linhas = tail (init bloco) -- Remove marcadores de início/fim

    -- Extrai campos simples (adicionado espaço após ': ' para match exato)
    nome <- extrairValor "Nome: " linhas
    classeStr <- extrairValor "Classe: " linhas
    classe' <- stringParaClasse classeStr
    racaStr <- extrairValor "Raça: " linhas
    raca' <- stringParaRaca racaStr
    forcaStr <- extrairValor "Forca: " linhas
    forca' <- readMaybe forcaStr :: Maybe Int
    intStr <- extrairValor "Inteligencia: " linhas
    int' <- readMaybe intStr :: Maybe Int
    dexStr <- extrairValor "Destreza: " linhas
    dex' <- readMaybe dexStr :: Maybe Int

    -- Extrai inventário
    let invLinhas = extrairSecao marcadorInicioInventario marcadorFimInventario linhas
    let inventario' = catMaybes $ map stringParaItem invLinhas

    -- Extrai história
    let histLinhas = extrairSecao marcadorInicioHistoria marcadorFimHistoria linhas
    let historia' = intercalate "\n" histLinhas

    return $ Personagem nome classe' raca' (Atributos forca' int' dex') inventario' historia'

-- Função auxiliar guard para Maybe
guard :: Bool -> Maybe ()
guard True = Just ()
guard False = Nothing

-- Divide o conteúdo do arquivo em blocos de personagem
splitBlocos :: [String] -> [[String]]
splitBlocos [] = []
splitBlocos linhas = 
    case findIndex (== marcadorInicioPersonagem) linhas of
        Nothing -> [] -- Nenhum personagem encontrado
        Just startIdx -> 
            let afterStart = drop (startIdx) linhas
            in case findIndex (== marcadorFimPersonagem) afterStart of
                Nothing -> [] -- Bloco incompleto (sem fim)
                Just endOffset -> 
                    let blocoAtual = take (endOffset + 1) afterStart
                        resto = drop (endOffset + 1) afterStart
                    in blocoAtual : splitBlocos resto

-- Carrega a lista de personagens do novo formato
carregarPersonagensFicha :: FilePath -> IO (Either String [Personagem])
carregarPersonagensFicha path = do
    resultado <- try (readFile path) :: IO (Either IOException String)
    case resultado of
      Left e -> return $ Left $ "Erro ao ler o arquivo '" ++ path ++ "': " ++ show e
      Right conteudo
        | null (trim conteudo) -> return $ Right [] -- Arquivo vazio
        | otherwise -> do
            let linhas = lines conteudo
            let blocos = splitBlocos linhas
            -- putStrLn $ "DEBUG: Blocos encontrados: " ++ show (length blocos) -- Debug
            -- mapM_ (putStrLn . ("Bloco: " ++) . show) blocos -- Debug

            let parseResults = zip [1..] $ map blocoParaPersonagem blocos
            -- putStrLn $ "DEBUG: Resultados do parse (Just/Nothing): " ++ show (map (isJust . snd) parseResults) -- Debug

            let (erros, personagens) = partitionEithers $ map (\(idx, res) -> case res of
                                                                              Just p -> Right p
                                                                              Nothing -> Left idx) parseResults

            if null erros
              then return $ Right personagens
              else return $ Left $ "Erros ao parsear personagens nos blocos de índice (começando em 1): " ++ show erros ++ ". Total de " ++ show (length erros) ++ " erros."

