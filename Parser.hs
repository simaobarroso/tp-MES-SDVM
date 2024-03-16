
module Parser where

-- import Prelude hiding ((<*>), (<$>))
import Data.Char


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

---------- Parsers ------------

-- FIXME
pListasIntHaskell = 
    enclosedBy (symbol '[') 
               (separatedBy pInt (symbol ',') ) 
               (symbol '[')

blocoCodigoC = 
    enclosedBy (symbol '{')
               (followedBy pInt (symbol ';'))
               (symbol '}')


pInt = f <$$> pSinal <**> pDigitos
    where f x y = x:y

pSinal = symbol  '-'
      <|> symbol  '+'
      <|> yield '+'

pInt4 = f <$$> optional (pSinal) <**> oneOrMore ( satisfy isDigit)
    where  f a b = a ++ b


pDigitos =  f <$$> satisfy isDigit
        <|> g <$$> satisfy isDigit <**> pDigitos
      where f d = [d]
            g d ds = d:ds

pString = f <$$> symbol '\"' 
            <**> zeroOrMore (satisfy (/='\"')) 
            <**> symbol '\"'
    where f a b c = b 
