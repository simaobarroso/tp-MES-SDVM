module GenPicoC where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

import Test.QuickCheck  hiding ((><))
import Test.QuickCheck.Function 


import Control.Monad

import Data.List
import Data.Char

import Picoc

type Gerador st a = StateT st Gen a

executar :: st -> Gerador st a -> Gen a
executar st g = evalStateT g st

split f g x = (f x, g x)
f >< g = split (f . fst) (g . snd)

--------------------------------------------------------------------------------
genInt :: Gen Int
genInt = arbitrary

sta     = suchThat arbitrary 

alpha   = listOf $ sta myisAlphaNum

genNome = (:) <$> (sta primeiraletra) <*> (take 4 <$> alpha)


commom_o e = [ (1, lift $ Neg  <$> e ),
               (1, get >>= lift . (Fetch  <$>) . elements),
               (1, lift $ Add  <$> e <*> e),
               (1, lift $ Sub  <$> e <*> e),
               (1, lift $ Mult <$> e <*> e)]

commom_b e = [ (1, Bigger  <$> e <*> e),
               (1, Smaller <$> e <*> e),
               (1, Equal   <$> e <*> e) ]

-- Generator of Int Expressions 
gei n = frequency $ (4, Const <$> arbitrary) :
      (( id >< executar vars) <$> (commom_o $ gei $ n-1))

-- Generator of Bool Expressions 
geb n = frequency $ (div 10 (n+1), B    <$> arbitrary) : (commom_b exp)  ++
      (( id >< executar vars) <$> (commom_o exp))

      where exp = geb $ div n 2

exe = executar vars

-- Generator of String Expressions 
ges n = frequency $ (4, Char <$> alpha ) :
     (( id >< executar vars) <$> (commom_o $ ges $ n-1 ))


--genType = elements ["int", "char", "bool"]
--
--genPico  v n = Pico <$> genBloco v n
--
--genBloco v n = vectorOf n $ genInst v (div n 2)

--genExp = get >>= \v -> oneof $ sized <$> ($v) <$> [gei, geb, ges] 


--
--genInst  v n = frequency [ (1, Atrib  <$> genNome <*> genType <*> genExp v ), -- colocar genNome dentro do v
--                           (1, While  <$> gbool   <*> gB       ),
--                           (1, If     <$> gbool   <*> gB       ),
--                           (1, IfElse <$> gbool   <*> gB <*> gB)
--                         ] 
--                where gB    = genBloco v n
--                      gbool = geb v n 
------- TESTES ------------------------------------------------------------------
--
--testegei   = sample $ sized $ gei vars
--testegeb   = sample $ sized $ geb vars
--testeges   = sample $ sized $ ges vars
--testegeins = sample $ sized $ genBloco vars

vars = map (("var_"++).show) [0..10]



