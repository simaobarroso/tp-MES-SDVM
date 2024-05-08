{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data
import Data.Generics.Zipper

import Library.Ztrategic
import Library.StrategicData (StrategicData)

import Control.Monad

import System.Random

import Picoc
import Types
import ExemplosPico

instance StrategicData Int
instance StrategicData PicoC
instance StrategicData a => StrategicData [a]

optC :: PicoC -> PicoC
optC (Pico c) = fromZipper res
    where
        e = toZipper novoP
        -- ⚠  Não meter optimizações do mesmo tipo ou usar o adhocTpSeq
        s = failTP `adhocTP` zopt `adhocTP` zi
        Just res =  applyTP (innermost s) e
        novoP = Pico $ filter (/= Idle) c

zi ( IfElse (Neg e) b b2)             = Just $ (IfElse e b2 b)
zi ( IfElse e b [] )                  = Just $ If e b 
zi ( While (B False) b )              = Just Idle
zi ( If    (B False) b )              = Just Idle
zi ( Comment a )                      = Just Idle
zi ( IfElse (B False ) b b2 )         = Just $ IfElse (B True) b2 b
zi ( IfElse (B True ) b b2 )          = Just $ If (B True) b
zi _ = Nothing

zopt :: Exp -> Maybe Exp
zopt (Neg (Neg e))        = Just e

zopt (Equal (B True) e )  = Just e
zopt (Equal e (B True) )  = Just e
zopt (Equal e (B False))  = Just $ Neg e
zopt (Equal (B False) e)  = Just $ Neg e
zopt _ = Nothing

-------------------------------------------------------------------------------
-- Mutations
-------------------------------------------------------------------------------

mutExp (Bigger e e2)    = Just (Equal e e2)
mutExp (Smaller e e2)   = Just (Bigger e e2)
mutExp _ = Nothing

mutInst ( IfElse e b b2) = Just (IfElse e b2 b)
mutInst _ = Nothing


applyMutation p f = do  
        let z = toZipper p
        novoz <- once_RandomTP z f
        return $ fromZipper novoz 

-- aplica uma mutação as expressoes ou as instruções 50/50
applyMPico p = do
        result <- rf
        if result > 0.5
        then applyMutation p mutExp
        else applyMutation p mutInst

-- para o programa do factorial, a2
tarefa6 = do 
    p <- applyMPico $ parser fact_test
    runTestSuite p a2input





