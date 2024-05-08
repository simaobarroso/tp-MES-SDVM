module Interpreter where

import Data.Char
import Data.Maybe
import Data.List

import Control.Concurrent

import System.Random (randomRIO)

import Library.Probability

import Types





-- maping functions of ariety 1
trimap f f2 f3 (L x) = L (f  x)
trimap f f2 f3 (M x) = M (f2 x)
trimap f f2 f3 (R x) = R (f3 x)

-- maping functions of ariety 2
trimap2 f f2 f3 (L x) (L x2) = L (f  x x2)
trimap2 f f2 f3 (M x) (M x2) = M (f2 x x2)
trimap2 f f2 f3 (R x) (R x2) = R (f3 x x2)

--  :: * * * * * -> bool
comp2 f f2 f3 (L x) (L x2) = M (f x x2) 
comp2 f f2 f3 (M x) (M x2) = M (f2 x x2) 
comp2 f f2 f3 (R x) (R x2) = M (f3 x x2) 

id2 = const

type    Context = [(String, Out String Bool Int )] 

cont :: Context
cont  = [("a", R 4), ("b", L "ola"), ("t", M True)]

-- EVAL BEHAVIOR - polimorfismo caseiro
-- Neg 4 = -4
-- Neg False = True
-- "asd" + "ola" = "asd" ++ "ola"
-- True  + False = True || False
--  3    + 8     = 3 + 8
--  3    * 8     = 3 * 8
-- True  * True  = True && True

eval :: Exp -> Context -> IO (Out String Bool Int)
eval (Const   i  ) _ = return $ R i
eval (Char    s  ) _ = return $ L s
eval (B       b  ) _ = return $ M b
eval (Fetch   a  ) c = return $ fromJust $ lookup a c
eval (D2      d  ) _ = R     <$> getFromDist d

eval (Neg     a  ) c = trimap  id   not  negate <$> eval a c
eval (Add     a b) c = trimap2 (++) (||) (+)    <$> eval a c <*> eval b c
eval (Mult    a b) c = trimap2 id2  (&&) (*)    <$> eval a c <*> eval b c
eval (Smaller a b) c = comp2   (<)  (<)  (<)    <$> eval a c <*> eval b c
eval (Bigger  a b) c = comp2   (>)  (>)  (>)    <$> eval a c <*> eval b c
eval (Equal   a b) c = comp2   (==) (==) (==)   <$> eval a c <*> eval b c

-------------------------------------------------------------------------------

wait = threadDelay . (* 1000000)

-- random float
rf = randomRIO (0,1) :: IO Float

getFromDist d = selectP d <$> rf
-- getFromDist (uniform [1..10]) 

-------------------------------------------------------------------------------
-- Running the code
-------------------------------------------------------------------------------

fromOut (M b) = b
fromOut _ = False

-- colocar na memória
put2 :: Inst ->  Context -> IO Context
put2 (Atrib n t e) c = do
    valor <- eval e c 
    return ((n, valor) : filter ( (/=n) . fst ) c) 

run :: [Inst] -> Context -> IO Context
run ((If e b):t)  c = do
    valor <- eval e c
    if fromOut valor
    then run (b  ++ t) c
    else run t c
run ((IfElse e b b2):t)  c = do
    valor <- eval e c
    if fromOut valor 
    then run (b  ++ t) c 
    else run (b2 ++ t) c

run (w@(While e b):t ) c = do
    valor <- eval e c
    if fromOut valor 
    then run (b ++ [w] ++  t) c 
    else run t c

run ((Wait k):t) c  = wait k >> run t c

run ((Print e):t) c = do 
    valor <- eval e c 
    putStr $ showOut valor
    run t c

run (atrib:t) c    = do
    nc <- put2 atrib c 
    run t nc 

run [] c = return c

runP (Pico p) = run p []
-------------------------------------------------------------------------------
-- Run debugging with prints of the intruction set
-------------------------------------------------------------------------------
-- Tarefa 7.

runDebug :: [Inst] -> Context -> IO Context
runDebug ((If e b):t)  c = do
    valor <- eval e c
    if fromOut valor
    then putStrLn ("If    :" ++ show e) >> runDebug (b  ++ t) c
    else runDebug t c

runDebug ((IfElse e b b2):t)  c = do
    valor <- eval e c
    if fromOut valor 
    then putStrLn ("If    :" ++ show e) >> runDebug (b  ++ t) c 
    else putStrLn ("ELSE  :" ++ show (Neg e)) >> runDebug (b2 ++ t) c

runDebug (w@(While e b):t ) c = do
    valor <- eval e c
    if fromOut valor 
    then putStrLn ("While :" ++ show e) >> runDebug (b ++ [w] ++  t) c 
    else runDebug t c

runDebug ((Wait k):t) c  = wait k >> run t c

runDebug ((Print e):t) c = do 
    valor <- eval e c 
    putStrLn $ "Print :" ++ show valor
    putStr $ showOut valor
    runDebug t c

runDebug (atrib:t) c    = do
    putStrLn $ "Atrib :" ++ show atrib
    nc <- put2 atrib c 
    runDebug t nc 

runDebug [] c = return c

runDebugP (Pico p) = runDebug p []
