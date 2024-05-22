
module Parser where

-- import Prelude hiding ((<*>), (<$>))
import Data.Char

import Types
import Library.Probability


infixl 2 <|>
infixl 3 <**>

type Parser r = String -> [(r, String)]

symbol :: Char -> Parser Char
symbol s [] = []
symbol s (h:t) | h == s  = [(s,t)]
               | otherwise = []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p []                = []
satisfy p (h:t) | p h       = [(h,t)]
                | otherwise = []

-- consome palavras reservadas
token :: String -> Parser String
token t [] = [] 
token t inp | t == a    = [(a,b)]
            | otherwise = []
    where (a,b) = (take k inp, drop k inp )
          k = length t

yield :: a -> Parser a
yield r inp = [ (r,inp)]

fail = yield []

-- esta função já existe
-- (++) . split? 
(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) inp = p inp ++ q inp


-- flip de >>= ? --- a -> r é um parser
(<**>) :: Parser (a -> r) -> Parser a -> Parser r
(p <**> q) inp = [ (f r, rst')
                 | (f  , rst ) <- p inp 
                 , (r  , rst') <- q rst 
                 ]

-- flip de fmap 
(<$$>) :: (a -> r) -> Parser a -> Parser r
(f <$$> p) inp = [ (f r, rst) | (r , rst) <- p inp ]


oneOrMore p = f <$$> p
          <|> g <$$> p <**> (oneOrMore p)
    where f  = return 
          g x y = x:y

zeroOrMore b = yield []
    <|> f <$$> b <**> (zeroOrMore b)
    where f x y  = x:y

optional p = yield []
        <|>  f <$$> p
    where f = return 

separatedBy p s = f <$$> p
              <|> g <$$> p <**> s <**> separatedBy p s
        where f a = [a]
              g x y z = x : z -- ignora o separador

followedBy p s = yield []
             <|> g <$$> p <**> s <**> (followedBy p s)
        where g x y z = x : z -- ignora o separador

enclosedBy a c f =  (\_ b _ -> b ) <$$> a <**> c <**> f

spaces = zeroOrMore $ satisfy isSpace

symbol'   a = (\k _ -> k)   <$$> symbol  a <**> spaces
symbol''  a = (\_ k _ -> k) <$$> spaces <**> symbol  a <**> spaces

token'    a = (\k _ -> k)   <$$> token   a <**> spaces
token''    a = (\_ k _ -> k) <$$> spaces <**> token   a <**> spaces

optional' a = (\k _ -> k)   <$$> optional a <**> spaces
optional'' a = (\_ k _ -> k)   <$$> spaces <**> optional a <**> spaces

satisfy'   a = (\k _ -> k)   <$$> satisfy a <**> spaces
satisfy''  a = (\_ k _ -> k) <$$> spaces <**> satisfy a  <**> spaces





----------------------------------------------------------------------------
primeiraletra a =  myisAlphaNum a && isLetter a && isLower a || a == '_'

myisAlphaNum  = flip elem $ '_':['a'..'z']++['A'..'Z']++['0'..'9']

----------- PARSER PICOC ------------------------------------------------------

pList :: Parser [Int]
pList = enclosedBy (symbol' '[') (separatedBy pInt2 (symbol ',')) (symbol' ']')

pInt2 :: Parser Int   
pInt2 = f <$$> pSinal2 <**> pDigitos
    where f x y = read $ x:y

pString = f <$$> symbol '\"' 
            <**> zeroOrMore (satisfy (/='\"')) 
            <**> symbol '\"'
    where f a b c = b 


pDigitos =  f <$$> satisfy isDigit
        <|> g <$$> satisfy isDigit <**> pDigitos
      where f d = [d]
            g d ds = d:ds

 
pSinal2 = symbol  '-' 
      <|> f <$$> symbol  '+'
      <|> f <$$> yield '+'
   where f = const '0'


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
     <|>  dist

dist = f <$$> symbol' 'D' <**> symbol' '(' 
                   <**> var   <**> symbol'' ','  
                   <**> pInt2 <**> symbol'' ','
                   <**> pInt2 <**> (symbol'' ')') 
    <|> g <$$> symbol' 'D' <**> symbol' '(' 
                   <**> var <**> symbol'' ',' <**> pList <**> (symbol'' ')') 
    where 
       f _ _ "uniform" _ a _ b _ = D2 (uniform [a..b])
       f _ _ _ _ a _ b _         = D2 (normal [a..b])
       g _ _ "uniform" _ l _ = D2 (uniform l)
       g _ _ _ _ l _         = D2 (normal l)

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

myprint = f <$$> token' "print" 
            <**> enclosedBy (symbol' '(') exp2  (symbol'' ')') 
    where f _ e  = Print e


-- ordem imperativa
ordem  =  atrib2 
      <|> while2
      <|> ifElse2
      <|> comment
      <|> myprint

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

exp0 =  f <$$> fator <**> symbol'' '*' <**> exp0
    <|> g <$$> fator <**> symbol'' '%' <**> exp0
    <|> j <$$> fator <**> symbol'' '/' <**> exp0
    <|> h <$$> symbol'' '~' <**> exp0
    <|>        fator
     where f e _ e2 = Mult e e2
           g e _ e2 = RDiv e e2
           j e _ e2 = Div e e2
           h _ e = Neg e

fator =         valor
     <|> f <$$> symbol' '(' <**> exp2 <**> symbol' ')'
     where f _ k _ = k

-------------------------------------------------------------------------------

parser = Pico . fst . head . filter (null . snd)  . linhas'

unparse (Pico i) = concatMap show i

getReturn = snd . head . filter ((=="return"). fst)

