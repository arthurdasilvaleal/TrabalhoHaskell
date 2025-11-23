-- Biblioteca simples de Máquina de Estados Finitos (FSM) em Haskell
-- Fornece uma API pequena para definir FSMs e executá-las

module FSM (
  FSM(..),
  runFSM,
  next,
  runFSMWithOutputs,
  composeFSMs
) where

-- Tipo FSM simples parametrizado por estado s, entrada i e saída o.
-- A função 'step' recebe o estado atual e a entrada, retornando
-- o novo estado e a saída correspondente.
newtype FSM s i o = FSM { step :: s -> i -> (s, o) }

-- Executa uma FSM sobre uma lista de entradas a partir de um estado inicial.
-- Retorna o estado final.
runFSM :: FSM s i o -> s -> [i] -> s
runFSM _ s [] = s
runFSM machine s (x:xs) = let (s', _) = step machine s x in runFSM machine s' xs

-- Executa uma FSM e coleta estados intermediários e saídas
-- Retorna a lista de (estadoDepois, saída) para cada passo
runFSMWithOutputs :: FSM s i o -> s -> [i] -> [(s, o)]
runFSMWithOutputs _ _ [] = []
runFSMWithOutputs machine s (x:xs) =
  let (s', o) = step machine s x
  in (s', o) : runFSMWithOutputs machine s' xs

-- Executa um passo e retorna (novoEstado, saída)
next :: FSM s i o -> s -> i -> (s, o)
next = step

-- Compoe duas FSMs que compartilham as mesmas entradas; a saída da
-- primeira pode ser usada ou ignorada pela segunda. Aqui permitimos a
-- composição retornando pares de estados e pares de saídas.
composeFSMs :: FSM s1 i o1 -> FSM s2 i o2 -> FSM (s1, s2) i (o1, o2)
composeFSMs f1 f2 = FSM $ \(s1, s2) input ->
  let (s1', o1') = step f1 s1 input
      (s2', o2') = step f2 s2 input
  in ((s1', s2'), (o1', o2'))
