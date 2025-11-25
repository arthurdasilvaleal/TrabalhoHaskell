module Main where

-- Este arquivo demonstra duas Máquinas de Estados Finitos (FSM):
-- 1) Uma porta automática com estados de abertura/fechamento e bloqueio.
-- 2) Uma máquina de venda simples que acumula crédito e dispensa um item.
-- Mostramos também: execução de sequências de entrada, verificações rápidas e composição de duas portas.

import FSM  -- Importa o tipo FSM e funções utilitárias (`runFSMWithOutputs`, `composeFSMs` etc.)

-- =============================
-- FSM 1: Porta Automática
-- =============================
-- Estados principais do ciclo: Fechada -> Abrindo -> Aberta -> Fechando -> Fechada
-- Estado especial: Bloqueada (interrompe qualquer operação normal até Desbloquear)
data PortaState = Fechada | Abrindo | Aberta | Fechando | Bloqueada deriving (Eq, Show)

-- Entradas possíveis:
-- Sensor       : Detecta presença, dispara abertura ou mantém aberta.
-- Timeout      : Indica passagem de tempo sem detecção (avança abertura -> aberta -> fechando -> fechada).
-- Bloquear     : Força o estado Bloqueada a partir de qualquer estado.
-- Desbloquear  : Sai de Bloqueada e retorna a Fechada.
data PortaInput = Sensor | Timeout | Bloquear | Desbloquear deriving (Eq, Show)

-- Saídas (Mensagens de status da transição)
data PortaOutput = Status String deriving (Eq, Show)

portaAutomatica :: FSM PortaState PortaInput PortaOutput
portaAutomatica = FSM $ \s input -> case (s, input) of
  -- Qualquer estado recebe Bloquear -> vai para Bloqueada.
  (_, Bloquear) -> (Bloqueada, Status "Porta bloqueada")
  -- De Bloqueada para Fechada ao Desbloquear.
  (Bloqueada, Desbloquear) -> (Fechada, Status "Porta desbloqueada -> Fechada")
  -- Enquanto Bloqueada, ignora demais entradas.
  (Bloqueada, _) -> (Bloqueada, Status "Porta está bloqueada")
  -- Fechada + Sensor: iniciar abertura.
  (Fechada, Sensor) -> (Abrindo, Status "Sensor detectado: Abrindo porta")
  -- Abrindo + Timeout: completou abertura.
  (Abrindo, Timeout) -> (Aberta, Status "Porta totalmente aberta")
  -- Aberta + Timeout: inicia fechamento por inatividade.
  (Aberta, Timeout) -> (Fechando, Status "Tempo expirado: Fechando porta")
  -- Aberta + Sensor: mantém-se aberta.
  (Aberta, Sensor) -> (Aberta, Status "Sensor detectado: Mantendo porta aberta")
  -- Fechando + Sensor: reverte fechamento e reabre.
  (Fechando, Sensor) -> (Abrindo, Status "Sensor detectado durante fechamento: Reabrindo")
  -- Fechando + Timeout: finaliza fechamento.
  (Fechando, Timeout) -> (Fechada, Status "Porta totalmente fechada")
  -- Caso padrão: qualquer outra combinação não altera estado.
  (_, _) -> (s, Status "Sem alteração")


-- =============================
-- FSM 2: Máquina de Venda
-- =============================
-- Estado SemCredito: nenhum valor acumulado.
-- Estado ComCredito Int: valor acumulado de moedas inseridas.
-- Item custa 2 unidades.
data VMState = SemCredito | ComCredito Int deriving (Eq, Show)

-- Entradas:
-- InserirMoeda n : adiciona n unidades de crédito.
-- Selecionar     : tenta comprar; exige crédito >= 2.
-- Cancelar       : devolve crédito acumulado se houver.
data VMInput = InserirMoeda Int | Selecionar | Cancelar deriving (Eq, Show)

-- Saídas textuais descritivas do passo.
data VMOutput = Mensagem String deriving (Eq, Show)

maquinaVenda :: FSM VMState VMInput VMOutput
maquinaVenda = FSM $ \s input -> case (s, input) of
  -- Inserção inicial de moeda: sai de SemCredito para ComCredito n.
  (SemCredito, InserirMoeda n) -> (ComCredito n, Mensagem $ "Inserido: " ++ show n)
  -- Acúmulo de crédito: soma ao valor existente.
  (ComCredito c, InserirMoeda n) -> (ComCredito (c + n), Mensagem $ "Total inserido: " ++ show (c + n))
  -- Seleção: sucesso se crédito >= 2, senão mensagem de insuficiência.
  (ComCredito c, Selecionar) -> if c >= 2
    then (SemCredito, Mensagem $ "Dispensado. Troco: " ++ show (c - 2))
    else (ComCredito c, Mensagem $ "Crédito insuficiente: " ++ show c)
  -- Cancelar: devolve crédito acumulado e volta para SemCredito.
  (ComCredito c, Cancelar) -> (SemCredito, Mensagem $ "Devolvido: " ++ show c)
  -- Selecionar sem crédito: nada acontece.
  (SemCredito, Selecionar) -> (SemCredito, Mensagem "Sem crédito")
  -- Cancelar sem crédito: sem efeito.
  (SemCredito, Cancelar) -> (SemCredito, Mensagem "Nada para cancelar")

-- Função utilitária para imprimir o trace de execução:
-- Recebe estado inicial e lista de pares (novoEstado, saída) produzidos a cada entrada.
printTrace :: (Show s, Show o) => s -> [(s, o)] -> IO ()
printTrace _ [] = putStrLn "(sem saídas)"
printTrace initState trace = do
  putStrLn $ "Inicial: " ++ show initState
  mapM_ (\(s, o) -> putStrLn $ "  -> estado: " ++ show s ++ ", saída: " ++ show o) trace

-- Impressão sem escapes para a porta (Status)
printTracePorta :: PortaState -> [(PortaState, PortaOutput)] -> IO ()
printTracePorta _ [] = putStrLn "(sem saídas)"
printTracePorta initState trace = do
  putStrLn $ "Inicial: " ++ show initState
  mapM_ (\(s, out) -> case out of
          Status msg -> putStrLn $ "  -> estado: " ++ show s ++ ", saída: Status \"" ++ msg ++ "\"") trace

-- Impressão sem escapes para a máquina (Mensagem)
printTraceMaquina :: VMState -> [(VMState, VMOutput)] -> IO ()
printTraceMaquina _ [] = putStrLn "(sem saídas)"
printTraceMaquina initState trace = do
  putStrLn $ "Inicial: " ++ show initState
  mapM_ (\(s, out) -> case out of
          Mensagem msg -> putStrLn $ "  -> estado: " ++ show s ++ ", saída: Mensagem \"" ++ msg ++ "\"") trace

-- Função simples de verificação: compara valor esperado e obtido.
-- Útil para demonstrar testes de unidade rudimentares inline.
assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "[OK] " ++ name
    else putStrLn $ "[FALHA] " ++ name ++ ": esperado " ++ show expected ++ ", obtido " ++ show actual

main :: IO ()
main = do
  -- Demonstra ciclo típico de abertura e fechamento sem bloqueio externo.
  putStrLn "Demonstração da porta automática (ciclo normal)"
  let inputs = [Sensor, Timeout, Timeout, Timeout]
      trace = runFSMWithOutputs portaAutomatica Fechada inputs
  printTracePorta Fechada trace

  -- Mostra reabertura quando sensor é detectado durante a fase de fechamento.
  putStrLn "\nPorta com sensor durante fechamento"
  let inputs2 = [Sensor, Timeout, Timeout, Sensor, Timeout]
      trace2 = runFSMWithOutputs portaAutomatica Fechada inputs2
  printTracePorta Fechada trace2

  -- Simula bloqueio e posteriores tentativas de interação até desbloquear.
  putStrLn "\nPorta bloqueada"
  let inputs3 = [Sensor, Bloquear, Sensor, Desbloquear, Sensor]
      trace3 = runFSMWithOutputs portaAutomatica Fechada inputs3
  printTracePorta Fechada trace3

  -- Mantém a porta aberta com múltiplas deteções de sensor seguidas.
  putStrLn "\nPorta mantida aberta por múltiplos sensores"
  let inputs4 = [Sensor, Timeout, Sensor, Sensor, Timeout, Timeout]
      trace4 = runFSMWithOutputs portaAutomatica Fechada inputs4
  printTracePorta Fechada trace4

  -- Demonstra ciclo e bloqueio no meio do fechamento.
  putStrLn "\nCiclo completo com bloqueio durante fechamento"
  let inputs5 = [Sensor, Timeout, Timeout, Bloquear, Desbloquear]
      trace5 = runFSMWithOutputs portaAutomatica Fechada inputs5
  printTracePorta Fechada trace5

  -- Testa que Timeout não altera a porta quando está bloqueada.
  putStrLn "\nTentativa de timeout em estado bloqueado"
  let inputs6 = [Bloquear, Timeout, Timeout, Desbloquear]
      trace6 = runFSMWithOutputs portaAutomatica Fechada inputs6
  printTracePorta Fechada trace6

  -- Cenários da máquina de venda (compra e cancelamento).
  putStrLn "\nDemonstração da máquina de venda"
  let vmInputs = [InserirMoeda 1, InserirMoeda 1, Selecionar]
      vmTrace = runFSMWithOutputs maquinaVenda SemCredito vmInputs
  printTraceMaquina SemCredito vmTrace

  putStrLn "\nCancelamento da máquina de venda"
  let vmInputs2 = [InserirMoeda 1, Cancelar]
      vmTrace2 = runFSMWithOutputs maquinaVenda SemCredito vmInputs2
  printTraceMaquina SemCredito vmTrace2

  -- Teste: Selecionar com crédito insuficiente (1 moeda, precisa 2).
  putStrLn "\nSelecionar com crédito insuficiente"
  let vmInputs3 = [InserirMoeda 1, Selecionar]
      vmTrace3 = runFSMWithOutputs maquinaVenda SemCredito vmInputs3
  printTraceMaquina SemCredito vmTrace3

  -- Teste: Inserir moeda grande e verificar troco (moeda 5, custo 2, troco 3).
  putStrLn "\nMoeda grande com troco"
  let vmInputs4 = [InserirMoeda 5, Selecionar]
      vmTrace4 = runFSMWithOutputs maquinaVenda SemCredito vmInputs4
  printTraceMaquina SemCredito vmTrace4

  -- Teste: Cancelar após inserir várias moedas (2 moedas de 1).
  putStrLn "\nCancelar após inserir 2 moedas"
  let vmInputs5 = [InserirMoeda 1, InserirMoeda 1, Cancelar]
      vmTrace5 = runFSMWithOutputs maquinaVenda SemCredito vmInputs5
  printTraceMaquina SemCredito vmTrace5

  -- Teste: Selecionar sem ter inserido nenhuma moeda.
  putStrLn "\nSelecionar sem crédito"
  let vmInputs6 = [Selecionar]
      vmTrace6 = runFSMWithOutputs maquinaVenda SemCredito vmInputs6
  printTraceMaquina SemCredito vmTrace6

  -- Teste: Cancelar sem ter inserido nenhuma moeda.
  putStrLn "\nCancelar sem crédito"
  let vmInputs7 = [Cancelar]
      vmTrace7 = runFSMWithOutputs maquinaVenda SemCredito vmInputs7
  printTraceMaquina SemCredito vmTrace7

  -- Verificações rápidas (testes simples de comportamento esperado).
  putStrLn "\nExecutando verificações rápidas..."
  -- Porta: estado inicial Fechada e Sensor deve ir para Abrindo.
  let t1 = runFSMWithOutputs portaAutomatica Fechada [Sensor]
  assertEq "Porta: Sensor -> Abrindo" [(Abrindo, Status "Sensor detectado: Abrindo porta")] t1
  
  -- Porta: Aberta + Sensor mantém Aberta.
  let t2 = runFSMWithOutputs portaAutomatica Aberta [Sensor]
  assertEq "Porta: Aberta + Sensor -> Aberta" [(Aberta, Status "Sensor detectado: Mantendo porta aberta")] t2
  
  -- Porta: Bloquear aplicado em Abrindo leva a Bloqueada.
  let t3 = runFSMWithOutputs portaAutomatica Abrindo [Bloquear]
  assertEq "Porta: Bloquear em qualquer estado" [(Bloqueada, Status "Porta bloqueada")] t3
  
  -- Máquina de venda: duas moedas permitem dispensar item com troco 0.
  let v1 = runFSMWithOutputs maquinaVenda SemCredito [InserirMoeda 1, InserirMoeda 1, Selecionar]
  assertEq "Máquina: 2 moedas -> Dispensado" [ (ComCredito 1, Mensagem "Inserido: 1") , (ComCredito 2, Mensagem "Total inserido: 2"), (SemCredito, Mensagem "Dispensado. Troco: 0") ] v1

  -- Máquina de venda: crédito insuficiente não dispensa.
  let v2 = runFSMWithOutputs maquinaVenda SemCredito [InserirMoeda 1, Selecionar]
  assertEq "Máquina: crédito insuficiente" [(ComCredito 1, Mensagem "Inserido: 1"), (ComCredito 1, Mensagem "Crédito insuficiente: 1")] v2

  -- Máquina de venda: moeda grande com troco.
  let v3 = runFSMWithOutputs maquinaVenda SemCredito [InserirMoeda 5, Selecionar]
  assertEq "Máquina: moeda 5 -> troco 3" [(ComCredito 5, Mensagem "Inserido: 5"), (SemCredito, Mensagem "Dispensado. Troco: 3")] v3

  -- Máquina de venda: cancelar devolve crédito.
  let v4 = runFSMWithOutputs maquinaVenda SemCredito [InserirMoeda 1, InserirMoeda 1, Cancelar]
  assertEq "Máquina: 2 moedas -> Cancelar devolve 2" [(ComCredito 1, Mensagem "Inserido: 1"), (ComCredito 2, Mensagem "Total inserido: 2"), (SemCredito, Mensagem "Devolvido: 2")] v4

  -- Máquina de venda: selecionar sem crédito.
  let v5 = runFSMWithOutputs maquinaVenda SemCredito [Selecionar]
  assertEq "Máquina: selecionar sem crédito" [(SemCredito, Mensagem "Sem crédito")] v5

  -- Máquina de venda: cancelar sem crédito.
  let v6 = runFSMWithOutputs maquinaVenda SemCredito [Cancelar]
  assertEq "Máquina: cancelar sem crédito" [(SemCredito, Mensagem "Nada para cancelar")] v6

  -- Composição: duas portas em paralelo processando a mesma sequência de entradas.
  putStrLn "\nPortas compostas (duas portas automáticas)"
  let composed = composeFSMs portaAutomatica portaAutomatica
      cTrace = runFSMWithOutputs composed (Fechada, Aberta) [Sensor, Timeout, Bloquear]
  putStrLn $ "Inicial composto: (Fechada, Aberta)"
  mapM_ (\((s1, s2), (o1, o2)) ->
    let sOut o = case o of Status msg -> "Status \"" ++ msg ++ "\""; _ -> show o
    in putStrLn $ "  -> estados: " ++ show s1 ++ "," ++ show s2 ++ " saídas: " ++ sOut o1 ++ "," ++ sOut o2) cTrace
