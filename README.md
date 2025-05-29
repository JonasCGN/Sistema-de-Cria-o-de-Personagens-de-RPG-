# Sistema de Criação de Personagens de RPG em Haskell

## Tema e Justificativa

O projeto consiste em um sistema para criar e gerenciar personagens de RPG, permitindo aplicar conceitos fundamentais de programação funcional, como tipos algébricos, listas, abstrações, ADT, modularização e IO. O tema foi escolhido por ser criativo, divertido e por exigir uma estrutura de dados rica, ideal para explorar os conceitos do curso.

## Estrutura do Projeto

- **Personagem.hs**: Define os tipos algébricos (Classe, Raça, Atributos, Item, Personagem) e funções de manipulação (adicionar, remover, buscar, listar, etc).
- **Nomeavel.hs**: Define uma classe de tipo personalizada para entidades que possuem nome.
- **main.hs**: Implementa a interface de usuário via terminal, persistência em arquivo e orquestra as operações do sistema.

## Funcionalidades

- Adicionar, remover, buscar e listar personagens e itens.
- Uso de listas, compreensão de listas e funções de ordem superior.
- Classe de tipo personalizada (`Nomeavel`).
- Encapsulamento do tipo principal como ADT.
- Modularização do código.
- Entrada e saída de dados via terminal e persistência em arquivo.

## Exemplos de Uso

- Adicionar um personagem: O usuário informa nome, classe, raça e atributos.
- Adicionar/remover itens do inventário de um personagem.
- Listar todos os personagens com seus detalhes.
- Salvar e carregar personagens de um arquivo.

## Comentários

O código está comentado para explicar o uso dos conceitos de programação funcional. O projeto pode ser expandido facilmente para incluir mais funcionalidades, como batalhas, magias, etc.