# FSM Haskell (Exemplo)

Este repositório contém uma pequena implementação de uma Maquina de Estados Finitos (FSM) em Haskell e dois exemplos:

 - Um controle de semáforo (semáforo).
 - Uma máquina de venda simples (máquina de venda).

## Estrutura
- `src/FSM.hs` : implementa o tipo `FSM` e as funções de execução
- `app/Main.hs` : arquivo `main` com exemplos
- `diagram/traffic_light.dot` : diagrama em GraphViz (opcional)

## Compilar e executar
Se tiver o `ghc` ou `runghc` instalado, execute: 

```powershell
# Compilar
ghc -isrc app/Main.hs -o fsm-demo

# Executar
./fsm-demo.exe

# Usando runghc (sem compilar)
runghc -isrc app/Main.hs
```

## Exemplos mostrados
 - Semáforo: avanço periódico no ciclo VERMELHO -> VERMELHO+AMARELO -> VERDE -> AMARELO -> VERMELHO; também existe um input `Emergencia` que retorna a luz ao vermelho imediatamente.
 - Máquina de venda: aceita moedas e dispensa com custo fixo (2 unidades nesse exemplo).

## Extensões sugeridas
- Transformar a máquina num `State` monad para integração com IO.
- Adicionar testes com `Hspec`/`tasty`.
- Exportar/ler diagramas em `.dot` e gerar imagens via Graphviz.

## Como funciona (visão rápida)

 - Exemplo de composição: `composeFSMs` permite rodar duas máquinas em paralelo (mesmo `i` de entrada), produzindo um par de estados e um par de saídas por passo. Isso é útil para modelar sistemas compostos (por exemplo, semáforos em interseção).

## Como expandir para o trabalho prático (dicas)

1. Documentar o diagrama de estados para cada máquina (semáforo, máquina de venda).
2. Mostrar as transições com exemplos de entradas (o `Main.hs` mostra isso).
3. Explicar as escolhas de modelagem: por que usar um `step` puro, como modelar entradas complexas, e como adicionar efeitos colaterais com `IO` (por exemplo, usando `StateT` ou `IORef`).

## Observações
Este projeto é uma base para a parte prática do trabalho: crie os diagramas, explique as transições, e apresente o código e saída passo-a-passo como pedido pelo trabalho.
