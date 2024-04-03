module GenPicoC where
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.Monad

import Data.List
import Data.Char

import Picoc

sta     = suchThat arbitrary 

alpha   = listOf $ sta myisAlphaNum

genNome = (:) <$> (sta primeiraletra) <*> (take 4 <$> alpha)

comum e k = [ (11, Neg    <$> e ),
              (11, Fetch  <$> elements k),
              (11, Add    <$> e <*> e),
              (11, Sub    <$> e <*> e),
              (11, Mult   <$> e <*> e),
              (1, Bigger  <$> e <*> e),
              (1, Smaller <$> e <*> e),
              (1, Equal   <$> e <*> e) ]
 
e f k n = f k $ div n 2

gei k 0 = Const <$> arbitrary
gei k n = frequency $ (20, Const <$> arbitrary) : comum (e gei k n)  k

geb k 0 = B     <$> arbitrary
geb k n = frequency $ (20, B     <$> arbitrary) : comum (e geb k n)  k 

ges k 0 = Char  <$> arbitrary
ges k n = frequency $ (20, Char  <$> alpha    ) : comum (e ges k n)  k

genExp v = oneof $ sized <$> ($v) <$> [gei, geb, ges] 

genType = elements ["int", "char", "bool"]

vars = map (("var_"++).show) [0..10]


-- FIXME corrivir variáveis e limitar tamanho 
genPico  v = Pico <$> genBloco v

genBloco v = vectorOf 1 $ genInst v

genInst  v = frequency [ (1, Atrib  <$> genNome <*> genType <*> genExp   v ), -- colocar genNome dentro do v
                         (1, While  <$> sized (geb v)       <*> genBloco v ),
                         (1, If     <$> sized (geb v)       <*> genBloco v ),
                         (1, IfElse <$> sized (geb v)       <*> genBloco v <*> genBloco v)
                       ] 


testegei = sample $ sized $ gei vars
testegeb = sample $ sized $ geb vars
testeges = sample $ sized $ ges vars
