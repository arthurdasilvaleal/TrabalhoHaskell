module Main where

import FSM

-- Estados: Vermelho -> VermelhoAmarelo -> Verde -> Amarelo -> Vermelho
data TLState = Vermelho | VermelhoAmarelo | Verde | Amarelo deriving (Eq, Show)
-- Exemplo de FSM: máquina de venda
  -- verificação rápida do semáforo: o primeiro pulso move para VermelhoAmarelo
data TLInput = Pulso | Emergencia deriving (Eq, Show)

data TLOutput = Luz String deriving (Eq, Show)

semaforo :: FSM TLState TLInput TLOutput
semaforo = FSM $ \s input -> case (s, input) of
  (_, Emergencia) -> (Vermelho, Luz "EMERGENCIA: VERMELHO")
  (Vermelho, Pulso) -> (VermelhoAmarelo, Luz "Vermelho+Amarelo")
  (VermelhoAmarelo, Pulso) -> (Verde, Luz "Verde")
  (Verde, Pulso) -> (Amarelo, Luz "Amarelo")
  (Amarelo, Pulso) -> (Vermelho, Luz "Vermelho")
  (_, _) -> (s, Luz "Sem alteração")


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
  putStrLn "Demonstração do semáforo (pulsos)"
  let inputs = replicate 6 Pulso
      trace = runFSMWithOutputs semaforo Vermelho inputs
  printTrace Vermelho trace

  putStrLn "\nSemáforo com emergência"
  let inputs2 = [Pulso, Pulso, Emergencia, Pulso]
      trace2 = runFSMWithOutputs semaforo Vermelho inputs2
  printTrace Vermelho trace2

  putStrLn "\nDemonstração da máquina de venda"
  let vmInputs = [InserirMoeda 1, InserirMoeda 1, Selecionar]
      vmTrace = runFSMWithOutputs maquinaVenda SemCredito vmInputs
  printTrace SemCredito vmTrace

  putStrLn "\nCancelamento da máquina de venda"
  let vmInputs2 = [InserirMoeda 1, Cancelar]
      vmTrace2 = runFSMWithOutputs maquinaVenda SemCredito vmInputs2
  printTrace SemCredito vmTrace2

  putStrLn "\nExecutando verificações rápidas..."
  -- verificação rápida do semáforo: o primeiro pulso move para RedAmber
  let t1 = runFSMWithOutputs semaforo Vermelho [Pulso]
  assertEq "Semáforo 1 pulso -> VermelhoAmarelo" [(VermelhoAmarelo, Luz "Vermelho+Amarelo")] t1
  -- verificação da máquina de venda: inserir 1, inserir 1, selecionar -> troco 0
  let v1 = runFSMWithOutputs maquinaVenda SemCredito [InserirMoeda 1, InserirMoeda 1, Selecionar]
  assertEq "Máquina: 2 moedas -> Dispensado" [ (ComCredito 1, Mensagem "Inserido: 1") , (ComCredito 2, Mensagem "Total inserido: 2"), (SemCredito, Mensagem "Dispensado. Troco: 0") ] v1

  -- Exemplo: FSM composta: rodar dois semáforos em paralelo
  putStrLn "\nSemáforos compostos (duas luzes)"
  let composed = composeFSMs semaforo semaforo
      cTrace = runFSMWithOutputs composed (Vermelho, Verde) (replicate 3 Pulso)
  putStrLn $ "Inicial composto: (Vermelho, Verde)"
  mapM_ (\((s1, s2), (o1, o2)) -> putStrLn $ "  -> estados: " ++ show s1 ++ "," ++ show s2 ++ " saídas: " ++ show o1 ++ "," ++ show o2) cTrace
