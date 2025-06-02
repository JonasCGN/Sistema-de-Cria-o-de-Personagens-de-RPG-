
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
historiaAleatoria = [ "A garota da cicatriz estrelar. Nasceu sob o brilho de uma estrela cadente, e uma marca semelhante a uma constelação surgiu em seu pulso. Essa cicatriz, que pulsava suavemente à noite, era a chave para segredos celestiais que a guiavam em momentos de grande dúvida, conectando-a a um destino que se estendia muito além de sua pequena aldeia.", "O sussurro da floresta proibida. João sempre foi avisado para não entrar na floresta que margeava sua vila, mas a curiosidade o corroía. Um dia, ele desobedeceu, atraído por um suave sussurro vindo das árvores mais antigas, que pareciam chamá-lo pelo nome. Lá dentro, ele descobriu que as lendas sobre a floresta eram apenas meias verdades, e a floresta guardava um poder ancestral.", "O artefato esquecido do avô. Escondido no sótão empoeirado, entre caixas velhas e memórias esquecidas, uma caixa de madeira entalhada revelou um relógio de bolso incomum. Ao abri-lo, o tempo pareceu parar, e uma visão nítida de um passado distante surgiu em sua mente, mostrando o avô em uma aventura que ele nunca havia contado.", "A canção do espírito do rio. Nas margens do rio Sereno, onde as águas corriam mais claras, uma melodia etérea podia ser ouvida em noites de lua cheia. Apenas Ana, com seu coração puro, conseguia discernir as palavras dessa canção, que contava histórias de amores perdidos e de um antigo pacto entre o rio e as terras que ele nutria.", "O último grifo da montanha oculta. Rumores falavam de uma criatura majestosa e solitária que vivia nos picos mais altos das Montanhas Sombrias. Um jovem explorador, determinado a provar a existência do grifo, embarcou em uma jornada perigosa. No topo, não encontrou uma besta, mas um guardião alado com olhos tão antigos quanto o tempo, que o testou com enigmas e desafios.", "O pergaminho das profecias sussurradas. Em uma biblioteca ancestral, um pergaminho esquecido, com a tinta desbotada pelo tempo, caiu de uma prateleira empoeirada. Ao tocá-lo, o leitor sentiu um arrepio e começou a ver vislumbres do futuro, pequenos fragmentos de eventos que estavam prestes a se desenrolar. O desafio agora era decifrar essas visões antes que fosse tarde demais.", "O jardim que florescia segredos. No centro de uma cidade movimentada, havia um jardim murado que parecia ter vida própria. Suas flores exóticas floresciam apenas à noite, e suas pétalas sussurravam segredos para aqueles que sabiam ouvir. Diziam que cada flor continha uma memória ou uma verdade há muito enterrada, esperando para ser redescoberta.", "O elfo com um passado humano. Um elfo, com olhos que carregavam a melancolia de eras, vivia à margem de sua comunidade, sempre em silêncio. Ele guardava um segredo doloroso: uma vez fora humano, amaldiçoado ou abençoado com a imortalidade, e lutava para conciliar suas memórias humanas com sua nova existência élfica.","A sombra do feiticeiro renegado. Histórias de um feiticeiro que havia mergulhado nas artes proibidas assombravam as crianças. Seus poderes, diziam, eram usados para o mal. Mas um jovem aprendiz, ao investigar os rastros deixados pelo feiticeiro, descobriu que a verdade era mais complexa: ele havia se sacrificado para conter uma ameaça ainda maior.","O navio fantasma da névoa eterna. Em noites de neblina espessa, na costa rochosa, um navio com velas rasgadas e um brilho fantasmagórico aparecia do nada. Não era um navio de piratas ou de exploradores, mas de almas perdidas de um naufrágio antigo, que repetiam sua última viagem em busca de uma redenção que só poderia ser encontrada com a ajuda de um coração corajoso."]

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
