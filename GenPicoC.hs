module GenPicoC where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

import Test.QuickCheck  hiding ((><))
import Test.QuickCheck.Function 

import Data.List
import Data.Char

import Picoc

type Gerador st a = StateT st Gen a

executar :: st -> Gerador st a -> Gen a
executar st g = evalStateT g st

split f g x = (f x, g x)
f >< g = split (f . fst) (g . snd)

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------
sta     = suchThat arbitrary 

alpha   = listOf $ sta myisAlphaNum

genNome = (:) <$> (sta primeiraletra) <*> (take 4 <$> alpha)


commom e1 e2 v = [ (3, Fetch  <$> elements v),
                   (1, return $ Neg  e1     ),
                   (1, return $ Add  e1 e2  ),
                   (1, return $ Sub  e1 e2  ),
                   (1, return $ Mult e1 e2  )]

commom_b e1 e2 = [ (1, return $ Bigger  e1 e2),
                   (1, return $ Smaller e1 e2),
                   (1, return $ Equal   e1 e2) ]


-- Generator of INT Expressions 
gei 0 =  lift $ Const <$> arbitrary
gei n = do
    e1 <- gei (n-1)
    e2 <- gei (n-1)
    get >>= \v -> do
        r  <- lift $ frequency  $ (4, Const <$> arbitrary ) : commom e1 e2 v  
        return r

-- Generator of STRING Expressions 
ges 0 =  lift $ Char <$> alpha
ges n = do
    e1 <- ges (n-1)
    e2 <- ges (n-1)
    get >>= \v -> do
        r  <- lift $ frequency $ (4, Char <$> alpha) : commom e1 e2 v
        return r

-- Generator of BOOLEAN Expressions 
geb 0 =  lift $ B <$> arbitrary
geb n = do
    e1 <- geb (n-1)
    e2 <- geb (n-1)
    get >>= \v -> do
        r  <- lift $ frequency $ (4, B <$> arbitrary): commom e1 e2 v ++ commom_b e1 e2 
        return r

genExp v n = oneof $ executar v <$> ($div n 2) <$> [ ges, gei, geb ]

genExpBool   v n = executar v $ geb  $ div n 2
genExpInt    v n = executar v $ gei  $ div n 2
genExpString v n = executar v $ ges  $ div n 2

genType = elements ["int", "char", "bool"]

genAtrib n = do 
    nome <- lift $ genNome 
    tipo <- lift $ genType 
    vars <- get 
    case tipo of
        "int"  -> do
            exp <- lift $ genExpInt vars n
            put (nome:vars)
            return $ Atrib nome tipo exp
        "char" -> do
            exp <- lift $ genExpString vars n
            put (nome:vars)
            return $ Atrib nome tipo exp
        "bool" -> do
            exp <- lift $ genExpBool vars n
            put (nome:vars)
            return $ Atrib nome tipo exp

genPico :: Int -> Gen PicoC
genPico n = Pico <$> executar [] (genBloco n) 


genBloco :: Int -> StateT [String] Gen Bloco
genBloco n = do 
    inst <- replicateM n $ genInst (div n 2)
    return inst

genInst  n = do 
    let k = div n 2
    while  <- genWhile  k
    iff    <- genIf     k
    ifelse <- genIfElse k
    atrib  <- genAtrib  k 

    lift $ frequency [ (1, return while ), 
                       (1, return iff   ), 
                       (1, return ifelse), 
                       (4, return atrib )]


genWhile n = do 
    vars  <- get
    expb  <- lift $ genExpBool vars n
    bloco <- genBloco n
    return $ While expb bloco
    
genIf n = do
    vars  <- get
    expb  <- lift $ genExpBool vars n
    bloco <- genBloco n
    return $ If expb bloco
    
genIfElse n = do
    vars   <- get
    expb   <- lift $ genExpBool vars n
    bloco  <- genBloco n
    bloco2 <- genBloco n
    return $ IfElse expb bloco bloco2
    
------------------------------------------------------------------------------
-- Shrinking 
-------------------------------------------------------------------------------


shrinkExp (Fetch  a)     = [ Fetch a       ]
shrinkExp (Const  a)     = [ Const a' | a' <- shrink a]
shrinkExp (Char   s)     = [ Char  s' | s' <- shrink s]
shrinkExp (B      b)     = [ B     b' | b' <- shrink b ]
shrinkExp (Empty   )     = [               ]
shrinkExp (Neg    e)     = [e] ++ [ Neg e2   | e2 <- shrinkExp e ]
shrinkExp (Add e e2)     = [e] ++ [e2] ++
                           [ (Add e' e2)     | e'  <- shrinkExp e ] ++
                           [ (Add e  e2')    | e2' <- shrinkExp e2 ]
shrinkExp (Sub e e2)     = [e] ++ [e2] ++
                           [ (Sub e' e2)     | e'  <- shrinkExp e ] ++
                           [ (Sub e  e2')    | e2' <- shrinkExp e2 ]
shrinkExp (Mult e e2)    = [e] ++ [e2] ++
                           [ (Mult e' e2)    | e'  <- shrinkExp e ] ++
                           [ (Mult e  e2')   | e2' <- shrinkExp e2 ]
shrinkExp (Bigger e e2)  = [e] ++ [e2] ++
                           [ (Bigger e' e2)  | e'  <- shrinkExp e ] ++
                           [ (Bigger e  e2') | e2' <- shrinkExp e2 ]
shrinkExp (Smaller e e2) = [e] ++ [e2] ++
                           [ (Smaller e' e2)  | e'  <- shrinkExp e ] ++
                           [ (Smaller e  e2') | e2' <- shrinkExp e2 ]
shrinkExp (Equal e e2)   = [e] ++ [e2] ++
                           [ (Equal e' e2)   | e'  <- shrinkExp e ] ++
                           [ (Equal e  e2')  | e2' <- shrinkExp e2 ]

shrinkInst (Idle ) = []
shrinkInst ( Comment s ) = [ Comment s' | s' <- shrink s ]
shrinkInst ( If e b    ) = [ (If e' b)  | e' <- shrinkExp e] ++ 
                           [ (If e  b') | b' <- shrinkBloco b]


shrinkBloco  = undefined
-- FIXME FAZER OS RESTOS
--data Inst = Atrib  String Type Exp
--          | While  Exp Bloco
--          | IfElse Exp Bloco Bloco 
--          | If     Exp Bloco 
--          | Comment String
--          deriving (Data, Eq)

--type Bloco = [Inst]
--
--type Type = String





--------------- TESTES --------------------------------------------------------

testege   = sample $ sized $ genExp vars2

vars2 = map (("var_"++).show) [0..10]
