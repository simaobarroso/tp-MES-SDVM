module ExemplosPico where

--import Parser
import Types

import Library.Probability

-------------------------------------------------------------------------------
-- EXEMPLOS DEBUGGING
-------------------------------------------------------------------------------

--programaPrimos = "print(2) print(3) int i = 5; int aux; while (True) {aux = 0;while (aux < i /2) { if ( i  % 2 == 0) then {i = i+1; }else {aux = aux +1; } if (aux == i/2 -1) then { print ( i) }  } }"

primos = "print( \"primo: \") print(2) print(\"\n\") int i = 3; while (True){ int aux = 2; while ((~(aux>i)) * ~(i%aux==0)){aux=aux+1;}if(aux==i) then{ wait (1) print( \"primo: \") print(i) print(\"\n\") } else { print(\"Não é primo: \") print(i) print(\"\n\") } i = i+1; }"

programa3M_Final = "int dado1 = D (normal,[2,3,9,10]); if (((dado2 + dado1) % 2) == 0 ) then { return = \"par\";} else {return = \"impar\";}"

-- a soma de dois números par é par
programa8 = "int dado1 = D (normal,[2,4,6,8]); if (((dado2 + dado1)%2) == 0 ) then { return = \"par\";} else {return = \"impar\";}"  

programa2M_Final ="if(n==0) then{ int fact = 1; } else{ int i = 1; int fact = 1; while (i<n+2){ fact = (fact)*(i); i = i+1; }}return = fact;"

programa1M_Final = "if(a>b) then{ if(b>c) then{ m = b; } else{ m = c; } } else{ if(a>c) then{ m = a; } else{ m = c; } }print(\"O maior número é\") print(m) return = m;"

--r = run b []
 --   where (Pico b) =  getPico fact

dados :: PicoC
dados = Pico [
    Atrib "x" "int" $ D2 (uniform [1..10]),
    Atrib "y" "int" $ D2 (uniform [1..10]),
    Atrib "z" "int" $ Mult (Fetch "x") (Fetch "y")
    ]


fact = "int n = 15; if ( n == 0 ) then { int fact = 1; } else { int i = 1; int fact = 1; while ( i < n + 1 ) { fact = fact * i; i = i + 1; } } return = fact;"


fact_test = "if ( n == 0 ) then { int fact = 1; } else { int i = 1; int fact = 1; while ( i < n + 1 ) { fact = fact * i; i = i + 1; } } return = fact;"

fact_prettyprint = "int n = 15; if ( n == 0 ) then { int fact = 1; } else { int i = 1; int fact = 1; while ( i < n + 1 ) { fact = fact * i; print(\"i = \") print(i) print(\" fact = \") print(fact) print (\"\n\") i = i + 1; } } return = fact;"

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

--p = unparse eC3

programa0 = fact
programa_aula = "if (a > b) then { if ( a > c ) then { m = a; } else { m = c; } } else { if ( b > c )  then { m = b; } else { m = c; } } return = m; "



-------------------------------------------------------------------------------

ex6 = Pico [ IfElse ( B True ) [(Atrib "x" "int" (Bigger (Const 1) (Const 3)))]
            [ (Atrib "x" "int" (Bigger (Const 4) (Const 6))) ]]

ex2 =  Pico [(Atrib "x" "int" (Mult (Const 1) (Add (Const 0) (Const 1))))]
--ex  = getPico l5
ex3 = Pico [Atrib "margem" "int" (Add (Const 15) (Const 0)),
                       IfElse (Equal (Fetch "margem") (B False))
                       [Atrib "margem" "int" (Mult (Add (Const 0)(Const 4))(Add (Add (Const 23)(Const 0)) (Mult (Const 3)(Const 1))))]
                       [Atrib "margem" "int" (Const 0)]]

ex4 = Pico [If     (B False) [] , Atrib "i" "int" (Const 2)]
ex5 = Pico [IfElse (B False) [] [Atrib "k" "char" (Char "ola")], Atrib "i" "int" (Const 2)]
