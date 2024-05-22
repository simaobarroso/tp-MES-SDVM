module Picoc where

import Parser
import ExemplosPico
import Types
import Interpreter
import Transformacoes
-- import Data.Generics.Aliases
import Library.Probability

import Control.Monad
infixl 3 ...
-------------------------------------------------------------------------------
-- TAREFAS DOS ENUNCIADO FINAL
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Equivalence properties, parser . unparser
-------------------------------------------------------------------------------

pu = parser . unparse

(~) :: PicoC -> PicoC -> IO Bool
(Pico p) ~ (Pico q) = liftM2 (==) (run p []) (run q [])


-- f >>= k = \ r -> k (f r) r

-------------- EQUIVALENCIA DA ÁRVORE ----------------------
--prop1 πcoc = πcoc == pu πcoc
prop1 = pu >>= (==)

-------------- "EQUIVALENCIA" NA SEMANTICA -----------------
--prop2 πcoc = πcoc ~ pu πcoc
prop2 = pu >>= (~)

------------- EQUIVALENCIA NA ÁRVORE E SEMÂNTICA -----------

-- πcoc le-se PicoC, não é Peacock (pavão)

-- prop3 πcoc = fmap ( == prop1 πcoc) (prop2 πcoc)
prop3 = phoenix ((<$>) . (==)) prop1 prop2

-- phoenix combinator
-- https://blog.lahteenmaki.net/combinator-birds.html
phoenix = (ap .) . (.)


-------------------------------------------------------------------------------

-- tarefa 1 -> está no Interpreter.hs
-- tarefa 5 -> está no Transformacoes.hs
-- tarefa 7 -> está no Interpreter.hs
-- tarefa 9 -> está no Interpreter.hs 

-- tarefa 2 
runTest :: PicoC -> (Context, Out String Bool Int) -> IO Bool
runTest (Pico p) (i,r) = liftM2 (==) (return r) (getReturn <$> run p i )


-- tarefa 3 
runTestSuite :: PicoC -> [(Context, Out String Bool Int)] -> IO Bool
runTestSuite = allM . sequence ... map . runTest 
 
-- blackbird combinator (.: no Data.Composition) 
-- https://blog.lahteenmaki.net/combinator-birds.html
(...) = (.).(.)

allM  = liftM $ all id


-- tarefa 4 
programa1 = parser programa_aula
programa2 = parser fact_test
programa3 = parser programa8


teste1 = runTestSuite programa1 input1  -- programa da aula
input1 = [
    ([("a",R 4),          ("b", R 3)     , ("c", R 0)] ,R 4),
    ([("a",R (-100)),     ("b", R (-200)), ("c", R (-101))] ,R (-100)),
    ([("a",L "bacalhau"), ("b", L "atum"), ("c", L "arroz de frango")] ,L "bacalhau") ]

teste2 = runTestSuite programa2 input2
input2  = [([("n",R 0)],  R 1),
           ([("n",R 1)],  R 1),
           ([("n",R 15)], R 1307674368000)]

teste3 = runTestSuite programa3 input3
input3 = [ ([("dado2", R 123456890) ], L "par"),
           ([("dado2", R 8) ]        , L "par"),
           ([("dado2", R 1) ]        , L "impar") ]


-- tarefa 6

programa1M = applyMPico programa1
programa1Mut = parser programa1M_Final

programa2M = applyMPico programa2
programa2Mut = parser programa2M_Final

programa3M = applyMPico programa3
programa3Mut = parser programa3M_Final

-- tarefa 8 FIXME NÃO SEI QUE É ISTO


instrumentation :: PicoC -> PicoC
instrumentation = undefined 

-- Não vou defenir esta função, porque já existe um interpretador debug
-- que imprime no ecra a instrução que está a executar


instrumentedTestSuite :: PicoC ->  [(Context, Out String Bool Int)] -> Bool
instrumentedTestSuite = undefined 

