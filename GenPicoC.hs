module GenPicoC where
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.Monad

import Data.List
import Data.Char

import Picoc

--data PicoC = Pico Bloco
--
--type Bloco = [Inst]
--
--type Type = String

palavras = listOf $ elements ['a'..'z']

vars = map (("var_"++).show) [0..10]

comum e k = [ (4, liftM  Neg     e ),
              (4, liftM  Fetch   $ elements k),
              (4, liftM2 Add     e e),
              (4, liftM2 Sub     e e),
              (4, liftM2 Mult    e e),
              (1, liftM2 Bigger  e e),
              (1, liftM2 Smaller e e),
              (1, liftM2 Equal   e e) ]
 

gei k 0 = liftM Const arbitrary
gei k n = frequency $ (12, liftM Const arbitrary) : comum e k
    where  e = gei k $ div n 2

geb k 0 = liftM B arbitrary
geb k n = frequency $ (12, liftM  B    arbitrary) : comum e k 
   where e = geb k $ div n 2

ges k 0 = liftM Char arbitrary
ges k n = frequency $ (12, liftM Char  palavras ) : comum e k
    where e = ges k $ div n 2


testegei = sample $ sized $ gei vars
testegeb = sample $ sized $ geb vars
testeges = sample $ sized $ ges vars

genExp :: [String ] -> Gen Exp
genExp v = oneof $ sized <$> ($v) <$> [gei, geb, ges] 

--data Inst = Atrib  String Type Exp
--          | While  Exp Bloco
--          | IfElse Exp Bloco Bloco
--          | If     Exp Bloco

genType = elements ["int", "char", "bool"]

genInst = frequency [ (1, liftM4 Atrib genType genExp) ]
