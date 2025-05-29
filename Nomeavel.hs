module Nomeavel where

-- | Classe de tipo para entidades nomeáveis do RPG
class Nomeavel a where
    obterNome :: a -> String
