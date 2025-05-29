-- | Classe de tipo para entidades nomeáveis do RPG
class Nomeavel a where
    obterNome :: a -> String

instance Nomeavel Personagem where
    obterNome = nome

instance Nomeavel Item where
    obterNome = nomeItem
