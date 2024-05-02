{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}
module Picoc where

import Parser

import Data.Data
import Data.Char
import Data.Maybe
import Data.List

import Control.Concurrent
import Control.Monad

import System.Random (randomRIO)

import Library.Probability

infixl 3 ...


-- import Data.Generics.Aliases

------ AUX -----------------------------------------------------------------

myisAlphaNum  = flip elem $ '_':['a'..'z']++['A'..'Z']++['0'..'9']

----------------------------------------------------------------------------

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
          | Idle
          | Return Exp
          deriving (Data, Eq)

data Exp = D2 (Dist Int)
         | Const   Int
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
         deriving (Data)

instance Eq Exp where
    e1 == e2 = show e1 == show e2 

-- FIXME 

var = f <$$> satisfy primeiraletra <**> zeroOrMore (satisfy isAlphaNum) 
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

davalor   =  f <$$> var <**> optional' ( j <$$> symbol'' '=' <**> exp2 ) 
      where f n []  = Atrib n "" Empty
            f n exp = Atrib n "" (head exp)
            j a b = b

atrib2 =  f <$$> mytype <**> davalor <**> symbol' ';'
      <|> g <$$>             davalor <**> symbol' ';'
    where f t (Atrib n _ e) _ = Atrib n t e
          g a _  = a

--atrib2  = f <$$> optional'' mytype <**> var <**> optional' ( j <$$> symbol'' '=' <**> exp2 ) <**> symbol' ';'
--    where f t n [] _  = Atrib n (sh t) Empty
--          f t n exp _ = Atrib n (sh t) (head exp)
--          j a b = b

            
mytype =  (token' "char")
      <|> (token' "int" )
      <|> (token' "bool" )


primeiraletra a =  myisAlphaNum a && isLetter a && isLower a || a == '_'

-------------------------------------------------------------------------------
-- Alternativa para os if then elses
myif = f <$$> token'  "if" 
         <**> enclosedBy (symbol' '(') exp2  (symbol'' ')') 
         <**> token' "then" <**> (symbol' '{') 
         <**> linhas'       <**> (symbol' '}')
    where f _ e _ _ c _  = If e c

finalIf = f <$$> myif <**> optional' ( g <$$> token' "else"       <**> (symbol' '{') <**> 
                                      (h <$$> optional'(linhas') )<**> (symbol'' '}'))
    where g _ _ c _ = c
          f i [] = i
          f (If e c) c2 = IfElse e c (concat c2)
          h [] = []
          h l = concat l

ifElse2 = finalIf
-------------------------------------------------------------------------------
-- versão antiga do if else, tem 
ifElse = f <$$> token''  "if" 
           <**> enclosedBy (symbol' '(') exp2  (symbol'' ')') 
           <**> token'' "then" 
           <**> (symbol' '{') <**> linhas' <**> (symbol'' '}')
           <**> optional ( g <$$> 
                     token' "else" 
                <**> (symbol' '{') <**> linhas' <**> (symbol'' '}'))
    where f _ p _ _ c _ [] = If p c
          f _ p _ _ c _ k  = IfElse p c (concat k)
          g _ _ l _ = l

while2 = f <$$> token'' "while" 
          <**> enclosedBy (symbol' '(') exp2  (symbol'' ')') 
          <**> (symbol' '{') <**> linhas' <**> (symbol'' '}')

    where f _ e _ l _ = While e l

-- isPrint -- Unicode characters (letters, numbers, marks, punctuation, symbols and spaces).
comment =  f <$$> token "//" <**> zeroOrMore (satisfy isPrint) <**> token' "//"
    where f _ t _ = Comment t



-- ordem imperativa
ordem  =  atrib2 
      <|> while2
      <|> ifElse2
      <|> comment

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
   <|>        exp0
      where f e _ e2 = Add e e2
            g e _ e2 = Sub e e2

exp0 = f <$$> fator <**> symbol'' '*' <**> exp0
   <|> h <$$> symbol'' '~' <**> exp0
   <|>        fator
    where f e _ e2 = Mult e e2
          h _ e = Neg e

fator =         valor
     <|> f <$$> symbol' '(' <**> exp2 <**> symbol' ')'
     where f _ k _ = k


-------------------------------------------------------------------------------
-- Unparser 
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
    show ( Return  a )      = "return"  ++ show a ++ "\n" 

instance Show Exp where
    show (Char   a )    = show a 
    show (Fetch  a )    = a 
    show (Empty    )    = ""
    show (Const  a )    = show a
    show (B      a )    = show a 
    show (Sub     e e2) = show e ++ "-"  ++ show e2 
    show (Mult    e e2) = "(" ++ show e ++ ")" ++ "*" ++"(" ++ show e2 ++ ")"
    show (Add     e e2) = show e ++ "+"  ++ show e2
    show (Smaller e e2) = show e ++ "<"  ++ show e2 
    show (Bigger  e e2) = show e ++ ">"  ++ show e2 
    show (Equal   e e2) = show e ++ "==" ++ show e2 
    show (Neg     e   ) = "~( " ++ show e ++ " )"

-------------------------------------------------------------------------------

data Out l m r = L l | M m | R r
    deriving (Data, Show, Eq)
-- Out não é funtor, trifuntor
-- Funtor :: * -> *
-- usando o ```import Data.Generics.Aliases``` pode ser possível criar uma instancia funtor Out 
-- poderia criar um fmap 
-- extT :: (a -> a)  |The transformation we want to extend
--      -> (b -> b)  |The type-specific transformation
--      -> a         |The argument we try to cast to type `b`
--      -> a

-- maping functions of ariety 1
trimap f f2 f3 (L x) = L (f  x)
trimap f f2 f3 (M x) = M (f2 x)
trimap f f2 f3 (R x) = R (f3 x)

-- maping functions of ariety 2
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

-- EVAL BEHAVIOR - polimorfismo caseiro
-- Neg 4 = -4
-- Neg False = True
-- "asd" + "ola" = "asd" ++ "ola"
-- True  + False = True || False
--  3    + 8     = 3 + 8
--  3    * 8     = 3 * 8
-- True  * True  = True && True

eval :: Exp -> Context -> IO (Out String Bool Int)
eval (Const   i  ) _ = return $ R i
eval (Char    s  ) _ = return $ L s
eval (B       b  ) _ = return $ M b
eval (Fetch   a  ) c = return $ fromJust $ lookup a c
eval (D2      d  ) _ = do {v <- (getFromDist d); return (R v)}

eval (Neg     a  ) c = trimap  id   not  negate <$> eval a c
eval (Add     a b) c = trimap2 (++) (||) (+)    <$> eval a c <*> eval b c
eval (Mult    a b) c = trimap2 id2  (&&) (*)    <$> eval a c <*> eval b c
eval (Smaller a b) c = comp2   (<)  (<)  (<)    <$> eval a c <*> eval b c
eval (Bigger  a b) c = comp2   (>)  (>)  (>)    <$> eval a c <*> eval b c
eval (Equal   a b) c = comp2   (==) (==) (==)   <$> eval a c <*> eval b c

-------------------------------------------------------------------------------

wait = threadDelay . (* 1000000)

-- random float
rf = randomRIO (0,1) :: IO Float

getFromDist d = selectP d <$> rf
-- getFromDist (uniform [1..10]) 

-------------------------------------------------------------------------------
-- Running the code
-------------------------------------------------------------------------------

fromOut (M b) = b
fromOut _ = False

-- colocar na memória
put2 :: Inst ->  Context -> IO Context
put2 (Atrib n t e) c = do
    valor <- eval e c 
    return ((n, valor) : filter ( (/=n) . fst ) c) 

run :: [Inst] -> Context -> IO Context
run ((If e b):t)  c = do
    valor <- eval e c
    if fromOut valor
    then run (b  ++ t) c
    else run t c
run ((IfElse e b b2):t)  c = do
    valor <- eval e c
    if fromOut valor 
    then run (b  ++ t) c 
    else run (b2 ++ t) c

run (w@(While e b):t ) c = do
    valor <- eval e c
    if fromOut valor 
    then run (b ++ [w] ++  t) c 
    else run t c

run ((Wait k):t) c = wait k >> run t c

run (atrib:t) c    = do
    nc <- put2 atrib c 
    run t nc 

run [] c = return c

runP (Pico p) = run p []

-------------------------------------------------------------------------------

getPico = Pico . fst . head . filter (null . snd)  . linhas'

parser = getPico

unparse (Pico i) = concatMap show i

getReturn = snd . head . filter ((=="return"). fst)

-------------------------------------------------------------------------------
-- Equivalence property
-------------------------------------------------------------------------------

--prop1 p = p == (parser ( unparse p) )
prop1 (Pico p) (Pico q) = p ~ q

(~) :: [Inst] -> [Inst] -> IO Bool
p ~ q = liftM2 (==) (run p []) (run q [])


-------------------------------------------------------------------------------

runTest :: PicoC -> (Context, Out String Bool Int) -> IO Bool
runTest (Pico p) (i,r) = liftM2 (==) (return r) (getReturn <$> run p i )

--res = runTest (getPico fact) ([],(R 1307674368000))

-- blackbird combinator (.: no Data.Composition) 
(...) = (.).(.)

runTestSuite :: PicoC -> [(Context, Out String Bool Int)] -> IO Bool
runTestSuite = liftM (all id) . sequence ... map . runTest 


a = runTestSuite (parser programa1) [ ([("a",R 3),("b",R 9),("c",R 1)], R 9)  ,([("a",R 3),("b",R 0),("c",R 1)], R 3) ]

a2 = runTest (parser programa1 ) ([("a",R 3),("b",R 9),("c",R 1)], R 9) 

a3 = runTestSuite (parser "if ( a > b ) then { return = a;} else { return = b;}") a3input
a3input = [
    ([("a",R 4),          ("b", R 3)     ] ,R 4),
    ([("a",R (-100)),     ("b", R (-200))] ,R (-100)),
    ([("a",L "bacalhau"), ("b", L "atum")] ,L "bacalhau") ]


-------------------------------------------------------------------------------
-- EXEMPLOS DEBUGGING
-------------------------------------------------------------------------------

dados :: PicoC
dados = Pico [
    Atrib "x" "int" $ D2 (uniform [1..10]),
    Atrib "y" "int" $ D2 (uniform [1..10]),
    Atrib "z" "int" $ Mult (Fetch "x") (Fetch "y")
    ]


r = run b []
    where (Pico b) =  getPico fact

fact = "int n = 15; if ( n == 0 ) then { int fact = 1; } else { int i = 1; int fact = 1; while ( i < n + 1 ) { fact = fact * i; i = i + 1; } } return = fact;"

fact2 = "int n = 15; if ( n == 0 ) then { int fact = 1;} //isto é a função fatorial !!!// int i = 1; int fact = 1; while ( i < n + 1 ) { fact = fact * i; i = i + 1; } "


l4 = "int margem = 15 ; if ( margem > 30 ) then { margem = 4 * 2 + 3 ; } else { margem = margem + 4 ; }"

l5 = "int margem = 15 ; int b = 0 + 0 + 0 + 0;if ( margem > 30 ) then { margem = 4 * 2 + 3 ; } else { margem = margem + 4 ; }"

l  = "int a ; char b; a = 10; while ( a ) { b = a; a = 3; if (b) then { } else { } } "
l2 = "int a ; char b; a = 10*10; while ( a ) { b = a; a = 3;if (b) then { } else { } } "
l3 = "int a ; char b; a = 10*10; while ( a<100 ) { b = a; a = 3;if (b) then { } else { } } "
ast  = Mult  ( Add ( Const 4 ) (Const 4) ) (Const 10)
ast2 = Mult  ( Add ( Const 0 ) (Const 4) ) (Const 10)
ast3 = Add (Add (Neg (Const 4)) (Const 4)) (Const 5)


eC3 :: PicoC
eC3 = Pico [Atrib "margem" "int" (Add (Const 15) (Const 0)),
                       IfElse (Bigger (Fetch "margem") (Mult (Const 30) (Const 1)))
                       [Atrib "margem" "int" (Mult (Add (Const 0)(Const 4))(Add (Add (Const 23)(Const 0)) (Mult (Const 3)(Const 1))))]
                       [Atrib "margem" "int" (Const 0)]]


p2 = "int margem=15+0;\nif(margem>(30)*(1))\nthen{\nint margem=(0+4)*(23+0+(3)*(1));\n}\nelse{\nint margem=0;\n}\n"

p = unparse eC3

programa0 = fact
programa1 = "if (a > b) then { if ( b > c ) then { m = b; } else { m = c; } } else { if ( a > c )  then { m = a; } else { m = c; } } return = m; "
