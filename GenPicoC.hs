module GenPicoC where
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.Monad

import Data.List
import Data.Char

import Picoc

sta     = suchThat arbitrary 

alpha   = listOf $ sta myisAlphaNum

genNome = liftM2 (:) (sta primeiraletra) (take 4 <$> alpha)

comum e k = [ (8, liftM  Neg     e ),
              (8, liftM  Fetch   $ elements k),
              (8, liftM2 Add     e e),
              (8, liftM2 Sub     e e),
              (8, liftM2 Mult    e e),
              (1, liftM2 Bigger  e e),
              (1, liftM2 Smaller e e),
              (1, liftM2 Equal   e e) ]
 

e f k n = f k $ div n 2

gei k 0 = liftM Const arbitrary
gei k n = frequency $ (18, liftM Const arbitrary) : comum (e gei k n)  k

geb k 0 = liftM B arbitrary
geb k n = frequency $ (18, liftM  B    arbitrary) : comum (e geb k n)  k 

ges k 0 = liftM Char arbitrary
ges k n = frequency $ (18, liftM Char  alpha ) : comum (e ges k n)  k

testegei = sample $ sized $ gei vars
testegeb = sample $ sized $ geb vars
testeges = sample $ sized $ ges vars

genExp :: [String] -> Gen Exp
genExp v = oneof $ sized <$> ($v) <$> [gei, geb, ges] 

--data PicoC = Pico Bloco
--type Bloco = [Inst]
--type Type = String
--data Inst = Atrib  String Type Exp
--          | While  Exp Bloco
--          | IfElse Exp Bloco Bloco
--          | If     Exp Bloco

genType = elements ["int", "char", "bool"]

vars = map (("var_"++).show) [0..10]


genInst v = frequency [ (1, liftM3 Atrib genNome genType (genExp v) ) ]
