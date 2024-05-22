{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}

module Types where


import Library.Probability

import Data.Data


deriving instance Data (Dist Int)

data PicoC = Pico Bloco
    deriving (Data, Eq)

type Bloco = [Inst]

type Type = String

data Inst = Atrib  String Type Exp
          | While  Exp Bloco
          | IfElse Exp Bloco Bloco 
          | If     Exp Bloco 
          | Comment String
          | Wait   Int 
          | Print Exp
          | Idle
          deriving (Data, Eq)

data Exp = D2 (Dist Int)
         --D2 (Dist String)
         | Const   Int
         | Char    String
         | B       Bool
         | Empty
         | Neg     Exp
         | Fetch   String 
         | Add     Exp Exp
         | Sub     Exp Exp
         | Mult    Exp Exp
         | RDiv    Exp Exp
         | Div     Exp Exp
         | Bigger  Exp Exp
         | Smaller Exp Exp
         | Equal   Exp Exp
         deriving (Data,Eq)
         
data Out l m r = L l | M m | R r
    deriving (Data,Show, Eq)

-- Out não é funtor, trifuntor
-- Funtor :: * -> *
-- usando o ```import Data.Generics.Aliases``` pode ser possível criar uma instancia funtor Out 
-- poderia criar um fmap que faz casting ao tipo
-- extT :: (a -> a)  |The transformation we want to extend
--      -> (b -> b)  |The type-specific transformation
--      -> a         |The argument we try to cast to type `b`
--      -> a
         

--instance Eq Exp where
--    e1 == e2 = show e1 == show e2 
    
    
-------------------------------------------------------------------------------
-- Unparser -- Show instances
-------------------------------------------------------------------------------
    
instance Show PicoC where
    show (Pico l) = concatMap show l 

instance Show Inst where
    show ( IfElse e b b2)   = "if("++ show e ++ ")\nthen{\n" 
            ++ concatMap show b  ++ "}\nelse{\n" 
            ++ concatMap show b2 ++ "}\n"
    show ( If e b )         = "if("++ show e ++ ")\nthen{\n" ++ concatMap show b ++ "}\n" 
    show ( While e b)       = "while (" ++ show e  ++ "){\n    " ++ concatMap show b ++ "}"
    show ( Atrib e t Empty) = t ++ " " ++ e ++  ";\n"
    show ( Atrib e t v)     = t ++ " " ++ e ++ " = " ++ show v ++ ";\n"
    show ( Idle )           = "" 
    show ( Comment a )      = "//"  ++ a ++ "//\n" 
    show ( Print e   )      = "print("++show e++")\n"

instance Show Exp where
    show (Char   a )    = show a 
    show (Fetch  a )    = a 
    show (Empty    )    = ""
    show (Const  a )    = show a
    show (B      a )    = show a 
    show (D2     d )    = show d
    show (Sub     e e2) = show e ++ "-"  ++ show e2 
    show (RDiv    e e2) = show e ++ "%"  ++ show e2 
    show (Div     e e2) = show e ++ "/"  ++ show e2 
    show (Mult    e e2) = "(" ++ show e ++ "*" ++ show e2 ++ ")"
    show (Add     e e2) = show e ++ "+"  ++ show e2
    show (Smaller e e2) = show e ++ "<"  ++ show e2 
    show (Bigger  e e2) = show e ++ ">"  ++ show e2 
    show (Equal   e e2) = show e ++ "==" ++ show e2 
    show (Neg     e   ) = "~( " ++ show e ++ " )"

showOut (L s ) = s
showOut (M b ) = show b
showOut (R r ) = show r
