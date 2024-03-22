# PicoC

# If then Else
Libertamos o nosso else da instrução if then else por isso sempre que tenho um if não
preciso de ter um else obrigatório, passa a ser opcional.


Criamos um tipo Out que representa o nosso output no PicoC que é um trifuntor.
    * String
    * Integer
    * Bool

Implementamos um trimfmap para que fosse possível implementar polimorfismo sobre algumas funções básicas. Quando na árvore de parsing há diferentes valores posso aplicar funções diferentes. O comportamento polimorfico da nossa função de avaliação é o seguinte:
> Neg 4 = -4
> Neg False = True
> "asd" + "ola" = "asd" ++ "ola"
> True  + False = True || False
>  3    + 8     = 3 + 8
>  3    * 8     = 3 * 8
> True  * True  = True && True

# Code Smeels
Quando temos comentários ou casos de while (False) ou um if (False) retornamos um tipo Idle que representa uma ação nula. Que serão removidos todos os idle da nossa árvore.

    Simplificamos a comparação de igualdade de boleanos 
    * exp == True   -> exp
    * exp == False  -> Neg exp
    * Neg (Neg exp) -> exp

    Troca dos ramos caso a negação de uma condição
    IfElse (Neg e) b b2   -> IfElse e b2 b 

    Simplificação e remoção do else que agora é opcional.
    * ifElse (True ) b b1 -> If (True)  b 

    * ifElse (False) b b1 -> If (True)  b1


Achamos que era boa ideia retirar-mos muitas das otimizações sugeridas, como por exemplo:
> Add a + 0 = a
> Add a + 1 = a
> Mult a * 0 = 0
> Mult a 1 = a
> Add a b = a + b

Se temos os elementos neutros e absorventes da adição e a sua soma também teriamos que ter para os outros tipos implementados: String e Bool. O nosso código tornar-se-ia muito repetitivo. Também, estas "melhorias" parecem que não passão de uma função de avaliação para casos simples, decidimos que estas otimizações são demasiados destrutivas da árvore de parsing e por isso devem ser retiradas.


# Typecheking - TODO
Não estamos a fazer nenhum tipo de typecheking por isso será válido frases como char b = 2 * 2; 

# Correr o programa

Implementamos uma função que permite correr o código, que numa primeira etapa tenta remover codesmeels e depois executar.A função ``` runP :: PicoC -> Context -> Context ``` que devolve um contexto com todas as variáveis criadas durante o programa.

quando corremos o seguinte programa que calcula o fatorial de 15:
```
int n = 15; 
if ( n == 0 ) then {
    int fact = 1;
}
else {
    int i = 1; 
    int fact = 1;
    while ( i < n ) {
        fact = i * fact ;
        i = i + 1;
    }
}
```
ficamos com o contexto:
```
[("i",R 16),("fact",R 1307674368000),("n",R 15)] 

```
A o valor de fact é o resultado de correr o programa que calcula o fatorial de 15.
