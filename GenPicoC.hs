import Test.QuickCheck
import Test.QuickCheck.Function

import Control.Monad

import Data.List
import Data.Char


data PicoC = Pico Bloco

type Bloco = [Inst]

type Type = String

data Inst = Atrib  String Type Exp
          | While  Exp Bloco
          | IfElse Exp Bloco Bloco
          | If     Exp Bloco
          | Comment String
          | Idle

instance Arbitrary Exp where 
       arbitrary = frequency [
       (1, liftM  Const   arbitrary),
       (1, liftM  Char    arbitrary),
       (1, liftM  B       arbitrary),
       (1, return Empty            ),
       (1, liftM  Neg     arbitrary),
       (1, liftM  Fetch   arbitrary),
       (1, liftM  Add     arbitrary arbitrary),
       (1, liftM  Sub     arbitrary arbitrary),
       (1, liftM  Mult    arbitrary arbitrary),
       (1, liftM  Bigger  arbitrary arbitrary),
       (1, liftM  Smaller arbitrary arbitrary),
       (1, liftM  Equal   arbitrary arbitrary) ]
