{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Picoc


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

-- fazer tipo sequencia?
zi ( IfElse (B True ) b b2 )          = Just $ If (B True) b

zi _ = Nothing


-- omitidos de proposito, ver readme
--zopt (Add (Const 0)  e)         = Just $ e    
--zopt (Add  e (Const 0))         = Just $ e    
--zopt (Add  (Const a) (Const b)) = Just $ Const (a + b)
--zopt (Mult (Const 1) e2)        = Just $ e2                           
--zopt (Mult e2 (Const 1))        = Just $ e2                
--zopt (Mult (Const 0) e2)        = Just $ Const 0
--zopt (Mult e2 (Const 0))        = Just $ Const 0
--zopt (Neg (Const a))            = Just $ Const (-a)  

zopt :: Exp -> Maybe Exp
zopt (Neg (Neg e))        = Just e

zopt (Equal (B True) e )  = Just e
zopt (Equal e (B True) )  = Just e
zopt (Equal e (B False))  = Just $ Neg e
zopt (Equal (B False) e)  = Just $ Neg e
zopt _ = Nothing



ex2 =  Pico [(Atrib "x" "int" (Mult (Const 1) (Add (Const 0) (Const 1))))]
ex  = getPico l5
ex3 = Pico [Atrib "margem" "int" (Add (Const 15) (Const 0)),
                       IfElse (Equal (Fetch "margem") (B False))
                       [Atrib "margem" "int" (Mult (Add (Const 0)(Const 4))(Add (Add (Const 23)(Const 0)) (Mult (Const 3)(Const 1))))]
                       [Atrib "margem" "int" (Const 0)]]

ex4 = Pico [If     (B False) [] , Atrib "i" "int" (Const 2)]
ex5 = Pico [IfElse (B False) [] [Atrib "k" "char" (Char "ola")], Atrib "i" "int" (Const 2)]


