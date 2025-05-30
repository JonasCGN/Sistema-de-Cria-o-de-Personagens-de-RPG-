
-- | Módulo e funções para gerar um personagem aléatorio.

module Aleatorio (gerarPersonagemAleatorio) where

import Personagem

-- Listas para aleatorizar
todasClasses :: [Classe]
todasClasses = [ Barbaro, Bardo, Bruxo, Clerigo, Druida, Feiticeiro, Guerreiro, Ladino, Mago, Monge, Paladino, Patrulheiro]

todasRacas :: [Raca]
todasRacas =[ Humano, Elfo, Anão, Halfling, MeioOrc, MeioElfo, Gnomo, Draconato, Tiefling]

nomesAleatorios :: [String]
nomesAleatorios = [ "Arthan", "Lyra", "Drogan", "Thalia", "Merric", "Kael", "Seraph", "Nym", "Torak", "Elaith", "Varyn", "Zara", "Dorian", "Faelan", "Kaelen", "Liora", "Tarin", "Sylas", "Eryndor", "Myra", "Jorin", "Selene", "Theron", "Isolde", "Kieran", "Alaric", "Mira", "Nolan", "Elara", "Galen"]

historiaAleatoria :: [String]
historiaAleatoria = [ "nasceu em uma pequena vila.", "cresceu em uma floresta encantada.", "foi treinado por um velho sábio.", "descobriu um antigo artefato mágico.", "teve uma infância difícil, mas superou os desafios.", "encontrou um dragão adormecido nas montanhas.", "foi escolhido por uma deidade para uma missão sagrada.", "descobriu que possui habilidades mágicas únicas."]

-- Função para pegar uma história aleatória
pegarHistoriaAleatoria :: Int -> String
pegarHistoriaAleatoria seed = historiaAleatoria !! (range 0 (length historiaAleatoria - 1) seed)

-- Gerador Linear Congruente (LCG) para números pseudoaleatórios
-- Recebe uma seed e retorna (novo número gerado, nova seed)
lcg :: Int -> (Int, Int)
lcg seed = (next, next)
  where
    a = 1664525
    c = 1013904223
    m = 2 ^ 32
    next = (a * seed + c) `mod` m

-- Gera uma lista infinita de números pseudoaleatórios a partir de uma seed
gerarRandoms :: Int -> [Int]
gerarRandoms seed = let (r, seed') = lcg seed in r : gerarRandoms seed'

-- Função para pegar valor em intervalo [low..high] a partir do número pseudoaleatório n
range :: Int -> Int -> Int -> Int
range low high n = low + (n `mod` (high - low + 1))

-- Gera personagem a partir de seed + contador
gerarPersonagemAleatorio :: Int -> Personagem
gerarPersonagemAleatorio seed =
  let randoms = gerarRandoms seed

      nomeIdx = range 0 (length nomesAleatorios - 1) (randoms !! 0)
      classeIdx = range 0 (length todasClasses - 1) (randoms !! 1)
      racaIdx = range 0 (length todasRacas - 1) (randoms !! 2)
      historiaIdx = range 0 (length historiaAleatoria - 1) (randoms !! 6)

      f = range 1 20 (randoms !! 3)
      i = range 1 20 (randoms !! 4)
      d = range 1 20 (randoms !! 5)

      nomeAleatorio = nomesAleatorios !! nomeIdx
      classeAleatoria = todasClasses !! classeIdx
      racaAleatoria = todasRacas !! racaIdx
      atributos = Atributos f i d
      historia = pegarHistoriaAleatoria (randoms !! historiaIdx)

  in criarPersonagem nomeAleatorio classeAleatoria racaAleatoria atributos historia
