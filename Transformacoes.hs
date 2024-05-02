{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Picoc
import Control.Monad

import System.Random

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


applyMutation p = do  
        let z = toZipper p
        novoz <- once_RandomTP z mutExp
        return $ fromZipper novoz 


rf = randomRIO (0,1) :: IO Float

applyMutation2 p = do
        r <- rf 
        if ( r > 0.5)
        then do
            novoz <- once_RandomTP (toZipper p) mutExp
            return $ fromZipper novoz 
        else do
            novoz <- once_RandomTP (toZipper p) mutInst
            return $ fromZipper novoz 




-------------------------------------------------------------------------------

ex6 = Pico [ IfElse ( B True ) [(Atrib "x" "int" (Bigger (Const 1) (Const 3)))]
            [ (Atrib "x" "int" (Bigger (Const 4) (Const 6))) ]]

ex2 =  Pico [(Atrib "x" "int" (Mult (Const 1) (Add (Const 0) (Const 1))))]
ex  = getPico l5
ex3 = Pico [Atrib "margem" "int" (Add (Const 15) (Const 0)),
                       IfElse (Equal (Fetch "margem") (B False))
                       [Atrib "margem" "int" (Mult (Add (Const 0)(Const 4))(Add (Add (Const 23)(Const 0)) (Mult (Const 3)(Const 1))))]
                       [Atrib "margem" "int" (Const 0)]]

ex4 = Pico [If     (B False) [] , Atrib "i" "int" (Const 2)]
ex5 = Pico [IfElse (B False) [] [Atrib "k" "char" (Char "ola")], Atrib "i" "int" (Const 2)]


