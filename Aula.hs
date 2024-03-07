{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Picoc

instance StrategicData Int
instance StrategicData PicoC
instance StrategicData a => StrategicData [a]

ex2=  Pico [(Atrib "x" "int" (Mult (Const 1) (Add (Const 0) (Const 1))))]

ex = Pico $ getPico l5

examplePicoC3 = Pico [Atrib "margem" "int" (Add (Const 15) (Const 0)),
                       If (Bigger (Fetch "margem") (Mult (Const 30) (Const 1)))
                       [Atrib "margem" "int" (Mult (Add (Const 0)(Const 4))(Add (Add (Const 23)(Const 0)) (Mult (Const 3)(Const 1))))]
                       [Atrib "margem" "int" (Const 0)]]


optC :: PicoC -> PicoC
optC p = fromZipper res
    where
        e = toZipper p
        s = failTP `adhocTP` zopt 
        Just res =  applyTP (innermost s) e

--zopt e = Just $ opt e

zi :: Exp -> Maybe Exp
zi (Add (Const 0)  e)    = Just $ e   

zopt :: Exp -> Maybe Exp
zopt (Add (Const 0)  e)    = Just $ e    
zopt (Add  e (Const 0))    = Just $ e    
zopt (Add  (Const a) (Const b)) = Just $ Const (a + b)
zopt (Mult (Const 1) e2)   = Just $ e2                           
zopt (Mult e2 (Const 1))   = Just $ e2                
zopt (Mult (Const 0) e2)   = Just $ Const 0
zopt (Mult e2 (Const 0))   = Just $ Const 0
zopt (Neg (Neg (Const a))) = Just $ Const a  
zopt _ = Nothing



--data Foo = Foo deriving Data
--data Bar = Bar Int
--    | Counter String
--    deriving ( Show , Data )
--lista :: [Int]
--lista = [1,2,3]
--
--alteraLista :: [Int]
--alteraLista =
--    let listaZipper        = toZipper lista
--        (Just nodoUm)      = down' listaZipper
--        (Just listaDois )  = right nodoUm
--        (Just nodoDois  )  = down' listaDois
--        (Just dois )       = (getHole nodoDois) :: Maybe Int
--        nodoDoisAtualizado = setHole ( dois + 10 ) nodoDois
--    in 
--        fromZipper nodoDoisAtualizado
--
--
--
--alteraListaS :: [Int]
--alteraListaS =
--    let listaZipper = toZipper lista
--        step = idTP `adhocTP` alteraDois
--        Just listaNova = applyTP (full_tdTP step) listaZipper
--    in fromZipper listaNova
--
--alteraDois :: Int -> Maybe Int
--alteraDois 2 = Just 12
--alteraDois x = Just x
--
--alteraCodigo :: PicoC
--alteraCodigo = fromZipper res
--    where
--        et1 = toZipper exemplo
--        step1 = idTP `adhocTP` alteraVar1 -- `adhocTP` alteraVar2
--        Just res = applyTP (full_tdTP step1) et1
--
--alteraVar1 :: Exp -> Maybe Exp
--alteraVar1 (Const a) = Just (Const (a+1))
--alteraVar1 x = Just x
--
--alteraVar2 :: Inst -> Maybe Inst
--alteraVar2 (Dec s1 s2)   = Just (Dec s1 (alteraStr s2))
--alteraVar2 (Atrib s1 s2) = Just (Atrib (alteraStr s1) s2)
--alteraVar2 x = Just x

--alteraCodigoStr :: PicoC
--alteraCodigoStr = fromZipper res
--    where
--        et1 = toZipper exemplo
--        step1 = idTP `adhocTP` alteraVar3
--        Just res = applyTP (once_tdTP step1) et1
--
--alteraVar3 :: String -> Maybe String
--alteraVar3 s = Just (alteraStr s)
--
--alteraStr :: String -> String
--alteraStr s = ('v':'_':s)
--
--alteraCodigo2 :: PicoC
--alteraCodigo2 = fromZipper res
--    where
--        et1 = toZipper exemplo
--        step1 = idTP `adhocTP` alteraVar1 `adhocTP` alteraVar2
--        Just res = applyTP (once_buTP step1) et1
--
--
---- once: idTP -> failtTP + Just caso que nÃ£o entra -> Nothing
