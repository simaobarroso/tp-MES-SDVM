{-# LANGUAGE DeriveDataTypeable #-}
module Picoc where

import Parser

import Data.Data
import Data.Char
import Data.Maybe
import Data.List

import Control.Monad


-- import Data.Generics.Aliases

-- Para garantir terminação não podemos usar recursividade a esquerda

------ AUX -----------------------------------------------------------------

pInt2 :: Parser Int
pInt2 = f <$$> pSinal2 <**> pDigitos
    where f x y = read $ x:y

pSinal2 = symbol  '-'
      <|> f <$$> symbol  '+'
      <|> f <$$> yield '+'
   where f = const '0'

----------------------------------------------------------------------------


data PicoC = Pico Bloco
    deriving (Data)

type Bloco = [Inst]

type Type = String

data Inst = Atrib String Type Exp
          | While Exp Bloco
          | If    Exp Bloco Bloco 
          | Idle
          deriving (Data, Eq)

data Exp = Const   Int
         | Char    String
         | B       Bool
         | Empty
         | Neg     Exp
         | Fetch   String 
         | Add     Exp Exp
         | Sub     Exp Exp
         | Mult    Exp Exp
         | Bigger  Exp Exp
         | Smaller Exp Exp
         | Equal   Exp Exp
           deriving (Data, Eq)


var = f <$$> satisfy primeiraletra <**> zeroOrMore (satisfy (isAlphaNum))
    where f a b = a:b


bool2 :: Parser Bool
bool2 =  f <$$> (token' "True" )
     <|> f <$$> (token' "False" )
     where f = read

valor =   Const <$$> pInt2
     <|>  Char  <$$> pString
     <|>  B     <$$> bool2
     <|>  Fetch <$$> var

-- safe head function
sh [] = []
sh l = head l

atrib2  =  f <$$> optional mytype <**> var <**> optional' ( j <$$> symbol'' '=' <**> exp2 )
    where f t n []  = Atrib n (sh t) Empty
          f t n exp = Atrib n (sh t) (head exp)
          j a b = b

            
mytype =  (token' "char")
      <|> (token' "int" )
      <|> (token' "bool" )

primeiraletra a =  isLetter a && isLower a || a == '_'

atribuicoes = followedBy atrib2 (symbol' ';')

ifElse = f <$$> token'  "if" 
           <**>    enclosedBy (symbol' '(') exp2  (symbol'' ')') 
           <**> token' "then" 
           <**>    (symbol' '{') <**> linhas <**> (symbol'' '}')
           <**> token' "else" 
           <**>    (symbol' '{') <**> linhas <**> (symbol'' '}')
    where f _ p _ _ c _ _ _ c2 _ = [If p c c2]

while = f <$$> token' "while" 
          <**> enclosedBy (symbol' '(') exp2  (symbol'' ')') 
          <**> (symbol' '{') <**> linhas <**> (symbol'' '}')

    where f _ e _ l _ = [While e l]

-- ordem imperativa
ordem  =  atribuicoes
      <|> while
      <|> ifElse

linhas = f <$$> ordem <**> optional' ordem
    where f b [] = b
          f b bs = b ++ last bs

linhas' = oneOrMore ordem


exp2 =  f <$$> exp1 <**> symbol'' '>' <**> exp2 
    <|> g <$$> exp1 <**> symbol'' '<' <**> exp2
    <|> h <$$> exp1 <**> token'' "==" <**> exp2
    <|>        exp1
    where f e _ e2 = Bigger  e e2
          g e _ e2 = Smaller e e2
          h e _ e2 = Equal e e2

exp1 = f <$$> exp0 <**> symbol'' '+' <**> exp1
   <|> g <$$> exp0 <**> symbol'' '-' <**> exp1
   <|> h <$$> symbol'' '~' <**> exp1
   <|>        exp0
      where f e _ e2 = Add e e2
            g e _ e2 = Sub e e2
            h _ e = Neg e

exp0 = f <$$> fator <**> symbol'' '*' <**> exp0
   <|>        fator
    where f e _ e2 = Mult e e2

fator =         valor
     <|> f <$$> symbol' '(' <**> exp2 <**> symbol' ')'
     where f _ k _ = k


--------------------------------------------------------------------------------
-- Unparser 
--------------------------------------------------------------------------------

instance Show PicoC where
    show (Pico l) = concatMap show l 

instance Show Inst where
    show ( If e b b2) = "if ("++ show e ++ ")\nthen\n{" 
            ++ concatMap show b
            ++ "}\nelse\n{" 
            ++ concatMap show b2
            ++ "}\n"
    show ( While e b) = "while (" ++ show e  ++ "){\n    " ++ concatMap show b ++ "}"
    show ( Atrib e t Empty)     = t ++ " " ++ e ++  ";\n"
    show ( Atrib e t v)     = t ++ " " ++ e ++ " = " ++ show v ++ ";\n"
    show ( Idle )     = " idle " 

instance Show Exp where
    show (Char   a )    = show a 
    show (Fetch  a )    = a 
    show (Empty    )    = ""
    show (Const  a )    = show a
    show (B      a )    = show a 
    show (Sub     e e2) = show e ++ " - "  ++ show e2 
    show (Mult    e e2) = show e ++ " * "  ++ show e2
    show (Add     e e2) = show e ++ " + "  ++ show e2
    show (Smaller e e2) = show e ++ " < "  ++ show e2 
    show (Bigger  e e2) = show e ++ " > "  ++ show e2 
    show (Equal   e e2) = show e ++ " == " ++ show e2 
    show (Neg     e   ) = "~( " ++ show e ++ " )"

--------------------------------------------------------------------------------

-- unparser . parser = id
clean =  filter (not . isSpace) 

puId s = (clean s)  ==  (clean $ concatMap show (fst $ last $ linhas s) )

--------------------------------------------------------------------------------

data Out l m r = L l | M m | R r
    deriving (Data, Show, Eq)
-- Out não é funtor
-- Funtor :: * -> *
-- usando o ```import Data.Generics.Aliases``` pode ser possível criar uma instancia funtor Out 
-- poderia criar um fmap 
-- extT :: (a -> a)  |The transformation we want to extend
--      -> (b -> b)  |The type-specific transformation
--      -> a         |The argument we try to cast to type `b`
--      -> a

trimap f f2 f3 (L x) = L (f  x)
trimap f f2 f3 (M x) = M (f2 x)
trimap f f2 f3 (R x) = R (f3 x)

-- ariety 2
trimap2 f f2 f3 (L x) (L x2) = L (f  x x2)
trimap2 f f2 f3 (M x) (M x2) = M (f2 x x2)
trimap2 f f2 f3 (R x) (R x2) = R (f3 x x2)

--  :: * * * * * -> bool
comp2 f f2 f3 (L x) (L x2) = M (f x x2) 
comp2 f f2 f3 (M x) (M x2) = M (f2 x x2) 
comp2 f f2 f3 (R x) (R x2) = M (f3 x x2) 

id2 = const

type    Context = [(String, Out String Bool Int )] 
cont :: Context
cont  = [("a", R 4), ("b", L "ola"), ("t", M True)]

-- FUNCTIONS BEHAVIOR 
-- Neg 4 = -4
-- Neg False = True
-- "asd" + "ola" = "asd" ++ "ola"
-- True  + False = True || False
--  3    + 8     = 3 + 8
--  3    * 8     = 3 * 8
-- True  * True  = True && True

eval :: Exp -> Context -> Out String Bool Int 
eval (Const   i  ) _ = R i
eval (Char    s  ) _ = L s
eval (B       b  ) _ = M b
eval (Fetch   a  ) c = fromJust $ lookup a c
eval (Neg     a  ) c = trimap  id   not  negate (eval a c)
eval (Add     a b) c = trimap2 (++) (||) (+)    (eval a c) (eval b c)
eval (Mult    a b) c = trimap2 id2  (&&) (*)    (eval a c) (eval b c)
eval (Smaller a b) c = comp2   (<)  (<)  (<)    (eval a c) (eval b c)
eval (Bigger  a b) c = comp2   (>)  (>)  (>)    (eval a c) (eval b c)
eval (Equal   a b) c = comp2   (==) (==) (==)   (eval a c) (eval b c)

--------------------------------------------------------------------------------

opt :: Exp -> Exp
opt (Add (Const 0)  e)    = e
opt (Add  e (Const 0))    = e
opt (Mult (Const 1) e2)   = e2
opt (Mult e2 (Const 1))   = e2
opt (Mult (Const 0) e2)   = Const 0
opt (Mult e2 (Const 0))   = Const 0
opt (Neg (Neg (Const a))) = Const a

--------------------------------------------------------------------------------

fromOut (M b) = b
fromOut _ = False

put :: Inst -> Context -> Context
put (Atrib n t e) c = (n,eval e c) : filter ( (/=n) . fst ) c


run :: [Inst] -> Context -> Context
run ((If e b b2):t)  c = if fromOut $ eval e c
                         then run (b  ++ t) c
                         else run (b2 ++ t) c

run (w@(While e b):t ) c = if fromOut $ eval e c
                           then run (b ++ [w] ++  t) c
                           else run t c

run (atrib:t) c = run t $ put atrib c

run [] c = c

getPico = Pico . fst . last . linhas

--------------------------------------------------------------------------------
-- É POSSIVEL CORRER FATORIAL
r = run b []
    where (Pico b) =  getPico fact

-- int n = 15; 
-- if ( n == 0 ) then {
--     int fact = 1;
-- }
-- else {
--     int i = 1; 
--     int fact = 1;
--     while ( i < n ) {
--         fact = i * fact ;
--         i = i + 1;
--     }
-- }
fact = "int n = 15; if ( n == 0 ) then { int fact = 1; } else { int i = 1; int fact = 1; while ( i < n + 1 ) { fact = fact * i; i = i + 1; } }"


l4 = "int margem = 15 ; if ( margem > 30 ) then { margem = 4 * 2 + 3 ; } else { margem = margem + 4 ; }"
l5 = "int margem = 15 ; int b = 0 + 0 + 0 + 0;if ( margem > 30 ) then { margem = 4 * 2 + 3 ; } else { margem = margem + 4 ; }"
l = "int a ; char b; a = 10; while ( a ) { b = a; a = 3;if (b) then { } else { } } "
l2 = "int a ; char b; a = 10*10; while ( a ) { b = a; a = 3;if (b) then { } else { } } "
l3 = "int a ; char b; a = 10*10; while ( a<100 ) { b = a; a = 3;if (b) then { } else { } } "
ast  = Mult  ( Add ( Const 4 ) (Const 4) ) (Const 10)
ast2 = Mult  ( Add ( Const 0 ) (Const 4) ) (Const 10)
ast3 = Add (Add (Neg (Const 4)) (Const 4)) (Const 5)


runP (Pico p) = run p []

