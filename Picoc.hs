module Picoc where

import Parser
import ExemplosPico
import Types
import Interpreter






import Control.Monad



infixl 3 ...

-- import Data.Generics.Aliases


-------------------------------------------------------------------------------
-- Equivalence properties
-------------------------------------------------------------------------------

pu = parser . unparse

-- πcoc le-se PicoC, não é Peacock (pavão)

-- equivalencia da árovore 
prop1 πcoc = πcoc == pu πcoc

-- "equivalencia" na semantica
prop2 πcoc = πcoc ~ pu πcoc

-- equivalencia nos dois
prop3 πcoc = do b <- prop2 πcoc; return $ (prop1 πcoc) == b


(~) :: PicoC -> PicoC -> IO Bool
(Pico p) ~ (Pico q) = liftM2 (==) (run p []) (run q [])

equivalence p q = p ~ q


-------------------------------------------------------------------------------
-- Tarefas dos enunciado final
-------------------------------------------------------------------------------

runTest :: PicoC -> (Context, Out String Bool Int) -> IO Bool
runTest (Pico p) (i,r) = liftM2 (==) (return r) (getReturn <$> run p i )


-- blackbird combinator (.: no Data.Composition) 
(...) = (.).(.)

allM  = liftM $ all id

runTestSuite :: PicoC -> [(Context, Out String Bool Int)] -> IO Bool
runTestSuite = allM . sequence ... map . runTest 


a  = runTestSuite (parser programa1) [ ([("a",R 3),("b",R 9),("c",R 1)], R 9),
                                       ([("a",R 3),("b",R 0),("c",R 1)], R 3) ]


a2 = runTestSuite ( parser fact_test ) a2input

a2input = [([("n",R 0)],  R 1),
           ([("n",R 1)],  R 1),
           ([("n",R 15)], R 1307674368000)]

a3 = runTestSuite (parser "if ( a > b ) then { return = a;} else { return = b;}") a3input

a3input = [
    ([("a",R 4),          ("b", R 3)     ] ,R 4),
    ([("a",R (-100)),     ("b", R (-200))] ,R (-100)),
    ([("a",L "bacalhau"), ("b", L "atum")] ,L "bacalhau") ]

--res = runTest (parser fact) ([],(R 1307674368000))

instrumentation :: PicoC -> PicoC
instrumentation = undefined 


-- Não vou defenir esta função, porque já existe um interpretador debug
-- que imprime no ecra a instrução que está a executar


instrumentedTestSuite :: PicoC ->  [(Context, Out String Bool Int)] -> Bool
instrumentedTestSuite = undefined 


