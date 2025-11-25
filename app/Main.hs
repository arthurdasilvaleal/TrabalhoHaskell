module Main where

import FSM

-- Exemplo de FSM: Porta Automática
-- Estados: Fechada -> Abrindo -> Aberta -> Fechando -> Fechada
-- Pode ser bloqueada em qualquer estado
data PortaState = Fechada | Abrindo | Aberta | Fechando | Bloqueada deriving (Eq, Show)

data PortaInput = Sensor | Timeout | Bloquear | Desbloquear deriving (Eq, Show)

data PortaOutput = Status String deriving (Eq, Show)

portaAutomatica :: FSM PortaState PortaInput PortaOutput
portaAutomatica = FSM $ \s input -> case (s, input) of
  (_, Bloquear) -> (Bloqueada, Status "Porta bloqueada")
  (Bloqueada, Desbloquear) -> (Fechada, Status "Porta desbloqueada -> Fechada")
  (Bloqueada, _) -> (Bloqueada, Status "Porta está bloqueada")
  (Fechada, Sensor) -> (Abrindo, Status "Sensor detectado: Abrindo porta")
  (Abrindo, Timeout) -> (Aberta, Status "Porta totalmente aberta")
  (Aberta, Timeout) -> (Fechando, Status "Tempo expirado: Fechando porta")
  (Aberta, Sensor) -> (Aberta, Status "Sensor detectado: Mantendo porta aberta")
  (Fechando, Sensor) -> (Abrindo, Status "Sensor detectado durante fechamento: Reabrindo")
  (Fechando, Timeout) -> (Fechada, Status "Porta totalmente fechada")
  (_, _) -> (s, Status "Sem alteração")


-- Exemplo de FSM: máquina de venda
-- Aceita moedas e pode dispensar um item que custa 2 unidades
data VMState = SemCredito | ComCredito Int deriving (Eq, Show)

data VMInput = InserirMoeda Int | Selecionar | Cancelar deriving (Eq, Show)

data VMOutput = Mensagem String deriving (Eq, Show)

maquinaVenda :: FSM VMState VMInput VMOutput
maquinaVenda = FSM $ \s input -> case (s, input) of
  (SemCredito, InserirMoeda n) -> (ComCredito n, Mensagem $ "Inserido: " ++ show n)
  (ComCredito c, InserirMoeda n) -> (ComCredito (c + n), Mensagem $ "Total inserido: " ++ show (c + n))
  (ComCredito c, Selecionar) -> if c >= 2
    then (SemCredito, Mensagem $ "Dispensado. Troco: " ++ show (c - 2))
    else (ComCredito c, Mensagem $ "Crédito insuficiente: " ++ show c)
  (ComCredito c, Cancelar) -> (SemCredito, Mensagem $ "Devolvido: " ++ show c)
  (SemCredito, Selecionar) -> (SemCredito, Mensagem "Sem crédito")
  (SemCredito, Cancelar) -> (SemCredito, Mensagem "Nada para cancelar")

printTrace :: (Show s, Show o) => s -> [(s, o)] -> IO ()
printTrace _ [] = putStrLn "(sem saídas)"
printTrace initState trace = do
  putStrLn $ "Inicial: " ++ show initState
  mapM_ (\(s, o) -> putStrLn $ "  -> estado: " ++ show s ++ ", saída: " ++ show o) trace

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "[OK] " ++ name
    else putStrLn $ "[FALHA] " ++ name ++ ": esperado " ++ show expected ++ ", obtido " ++ show actual

main :: IO ()
main = do
  putStrLn "Demonstração da porta automática (ciclo normal)"
  let inputs = [Sensor, Timeout, Timeout, Timeout]
      trace = runFSMWithOutputs portaAutomatica Fechada inputs
  printTrace Fechada trace

  putStrLn "\nPorta com sensor durante fechamento"
  let inputs2 = [Sensor, Timeout, Timeout, Sensor, Timeout]
      trace2 = runFSMWithOutputs portaAutomatica Fechada inputs2
  printTrace Fechada trace2

  putStrLn "\nPorta bloqueada"
  let inputs3 = [Sensor, Bloquear, Sensor, Desbloquear, Sensor]
      trace3 = runFSMWithOutputs portaAutomatica Fechada inputs3
  printTrace Fechada trace3

  putStrLn "\nPorta mantida aberta por múltiplos sensores"
  let inputs4 = [Sensor, Timeout, Sensor, Sensor, Timeout, Timeout]
      trace4 = runFSMWithOutputs portaAutomatica Fechada inputs4
  printTrace Fechada trace4

  putStrLn "\nCiclo completo com bloqueio durante fechamento"
  let inputs5 = [Sensor, Timeout, Timeout, Bloquear, Desbloquear]
      trace5 = runFSMWithOutputs portaAutomatica Fechada inputs5
  printTrace Fechada trace5

  putStrLn "\nTentativa de timeout em estado bloqueado"
  let inputs6 = [Bloquear, Timeout, Timeout, Desbloquear]
      trace6 = runFSMWithOutputs portaAutomatica Fechada inputs6
  printTrace Fechada trace6

  putStrLn "\nDemonstração da máquina de venda"
  let vmInputs = [InserirMoeda 1, InserirMoeda 1, Selecionar]
      vmTrace = runFSMWithOutputs maquinaVenda SemCredito vmInputs
  printTrace SemCredito vmTrace

  putStrLn "\nCancelamento da máquina de venda"
  let vmInputs2 = [InserirMoeda 1, Cancelar]
      vmTrace2 = runFSMWithOutputs maquinaVenda SemCredito vmInputs2
  printTrace SemCredito vmTrace2

  putStrLn "\nExecutando verificações rápidas..."
  -- verificação rápida da porta: sensor move de Fechada para Abrindo
  let t1 = runFSMWithOutputs portaAutomatica Fechada [Sensor]
  assertEq "Porta: Sensor -> Abrindo" [(Abrindo, Status "Sensor detectado: Abrindo porta")] t1
  
  -- verificação: porta mantém aberta quando sensor detectado
  let t2 = runFSMWithOutputs portaAutomatica Aberta [Sensor]
  assertEq "Porta: Aberta + Sensor -> Aberta" [(Aberta, Status "Sensor detectado: Mantendo porta aberta")] t2
  
  -- verificação: bloqueio funciona de qualquer estado
  let t3 = runFSMWithOutputs portaAutomatica Abrindo [Bloquear]
  assertEq "Porta: Bloquear em qualquer estado" [(Bloqueada, Status "Porta bloqueada")] t3
  
  -- verificação da máquina de venda: inserir 1, inserir 1, selecionar -> troco 0
  let v1 = runFSMWithOutputs maquinaVenda SemCredito [InserirMoeda 1, InserirMoeda 1, Selecionar]
  assertEq "Máquina: 2 moedas -> Dispensado" [ (ComCredito 1, Mensagem "Inserido: 1") , (ComCredito 2, Mensagem "Total inserido: 2"), (SemCredito, Mensagem "Dispensado. Troco: 0") ] v1

  -- Exemplo: FSM composta: rodar duas portas em paralelo
  putStrLn "\nPortas compostas (duas portas automáticas)"
  let composed = composeFSMs portaAutomatica portaAutomatica
      cTrace = runFSMWithOutputs composed (Fechada, Aberta) [Sensor, Timeout, Bloquear]
  putStrLn $ "Inicial composto: (Fechada, Aberta)"
  mapM_ (\((s1, s2), (o1, o2)) -> putStrLn $ "  -> estados: " ++ show s1 ++ "," ++ show s2 ++ " saídas: " ++ show o1 ++ "," ++ show o2) cTrace
