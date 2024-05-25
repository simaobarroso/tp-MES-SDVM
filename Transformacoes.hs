{-# LANGUAGE DeriveDataTypeable #-}

module Transformacoes where

import Data.Data
import Data.Generics.Zipper

import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Library.Probability

import Control.Monad

import System.Random

import Interpreter
--import Parser
import Types
--import ExemplosPico
--import GenPicoC

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

mutExp (Neg     a   ) = Just $ Neg (Neg( a))
mutExp (B       a   ) = Just $ Neg (B a)
mutExp (Const   a   ) = Just $ Const $ succ a
mutExp (Char    a   ) = Just $ Char  $ a ++ "🐱"
mutExp (D2      d   ) = Just $ D2    $ joinWith (+) d (Library.Probability.uniform [1..10]) 

mutExp (RDiv    e e2) = Just $ Add (RDiv e e2) (Const 1)
mutExp (Div     e e2) = Just $ Div e2 e
mutExp (Mult    e e2) = Just $ Mult e e2 -- está igual
mutExp (Sub     e e2) = Just $ Sub e2 e
mutExp (Add     e e2) = Just $ Add (Add e2 e) e
mutExp (Bigger  e e2) = Just $ Equal e e2
mutExp (Smaller e e2) = Just $ Bigger e e2
mutExp (Equal   e e2) = Just $ Neg $ Equal e e2
mutExp _              = Nothing


mutInst ( IfElse e b b2) = Just $ IfElse e b2 b
mutInst (Comment s     ) = Just $ Comment $ s ++ "αβλγ⚠ç!Ç!Ç!Ç!"
mutInst (Wait    n     ) = Just $ Wait $ succ n
mutInst (Print   e     )  = Just  $ Print $ fj $ mutExp e
mutInst (If      e b   ) = Just $ IfElse e b []
mutInst (While   e b   ) = Just $ While (Neg e) b 

mutInst _ = Nothing

fj (Just x) = x

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

