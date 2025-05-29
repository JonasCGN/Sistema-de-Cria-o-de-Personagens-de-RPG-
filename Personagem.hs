-- | Módulo de Tipos e Funções de Personagem para o sistema de RPG
module Personagem (
    Classe(..), Raca(..), Atributos(..), Item(..), Personagem,
    criarPersonagem, encontrarPersonagem, adicionarItem, removerItem,
    listarPersonagens, removerPersonagem, temItem, descricaoCompleta
) where

-- Tipos Algébricos

data Classe = Guerreiro | Mago | Ladino | Clerigo | Arqueiro deriving (Show, Eq)
data Raca = Humano | Elfo | Anao | Orc | Goblin deriving (Show, Eq)

data Atributos = Atributos {
  forca :: Int,
  inteligencia :: Int,
  destreza :: Int
} deriving (Show, Eq)

data Item = Item {
  nomeItem :: String,
  descricaoItem :: String
} deriving (Show, Eq)

data Personagem = Personagem {
  nome :: String,
  classe :: Classe,
  raca :: Raca,
  atributos :: Atributos,
  inventario :: [Item]
} deriving (Show, Eq)

-- Funções de manipulação
criarPersonagem :: String -> Classe -> Raca -> Atributos -> Personagem
criarPersonagem n c r a = Personagem n c r a []

encontrarPersonagem :: String -> [Personagem] -> Maybe Personagem
encontrarPersonagem n = foldr (\p acc -> if nome p == n then Just p else acc) Nothing

adicionarItem :: Item -> Personagem -> Personagem
adicionarItem item p = p { inventario = item : inventario p }

removerItem :: String -> Personagem -> Personagem
removerItem nomeIt p = p { inventario = filter ((/= nomeIt) . nomeItem) (inventario p) }

listarPersonagens :: [Personagem] -> [String]
listarPersonagens = map nome

removerPersonagem :: String -> [Personagem] -> [Personagem]
removerPersonagem n = filter ((/= n) . nome)

temItem :: String -> Personagem -> Bool
temItem nomeIt p = any ((== nomeIt) . nomeItem) (inventario p)

descricaoCompleta :: Personagem -> String
descricaoCompleta p = nome p ++ " (" ++ show (classe p) ++ ", " ++ show (raca p) ++ ")\nAtributos: " ++ show (atributos p) ++ "\nItens: " ++ show (map nomeItem (inventario p))
