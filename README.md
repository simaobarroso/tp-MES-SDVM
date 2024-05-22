# Relatório de Linguagem e Implementação de Interpretação

## João Alvim - pg53902 - 03/05/2024 
## João Alvim - pg53902 - 03/05/2024 
## João Alvim - pg53902 - 03/05/2024 

## Introdução

Este relatório descreve uma linguagem de programação e sua implementação de interpretação. A linguagem apresentada possui uma sintaxe definida e suporta várias características, incluindo estruturas de controle condicionais, loops, manipulação de variáveis e distribuições de valores. Além disso, o relatório detalha a implementação de um interpretador para executar programas escritos nesta linguagem. Também um gerador FIXME

## Definição da Linguagem

A linguagem é definida por meio de uma estrutura de dados em Haskell. A seguir, estão os principais componentes da linguagem:

### Tipos de Dados

- `Exp`: Representa expressões na linguagem, incluindo constantes, operações aritméticas, operações lógicas e operações de comparação.

- `Inst`: Define instruções executáveis, como estruturas condicionais (`IfElse`), loops (`While`), atribuição de valores (`Atrib`) e pausa (`Wait`).

### Características da Linguagem

- **Construtor If-Else Opcional**: A estrutura `IfElse` não exige um ramo `else`, tornando-o opcional para maior flexibilidade na codificação.

- **Distribuições de Valores**: Introduz o conceito de distribuições de inteiros (`Dist Int`), permitindo a avaliação de elementos com base em valores aleatórios que seguem uma distribuição específica.

- **Instrução Wait**: O construtor `Wait` permite esperar a execução do programa por um número especificado de segundos, utilizando a função `threadDelay :: Int -> IO ()`.

- **Output Polimórfico**: Define um tipo `Out` que representa a saída da linguagem, permitindo valores de tipo `String`, `Integer` e `Bool`. Isso possibilita uma saída flexível e adaptável às necessidades do programa.

- **Polimorfismo**: Implementa funções polimórficas básicas, como adição e multiplicação, para lidar com diferentes tipos de entrada de forma coerente.

## Implementação do Parser
### Novos combinadores de parsing 
Quando se tenta fazer um parser para consumir várias palavras:

> palavras = zeroOrMore (satisfy' isAlphaNum)

Temos que consome uma string com espaços e devolve as palavras concatenadas sem os espaços
> palavras "a b" = [("","a b"),("a"," b"),("a","b"),("ab","")]

Mas se usar o satisfy sem a plica temos que:
> palavras "a b" = [("","a b"),("a"," b")]

neste caso fico com um espaço no início da linha o parsing não termina.

Por isso é útil consumir espaços não só fim com o no início. Surge a necessidade de criar novos combinadores:

```
symbol''   a = (\_ k _ -> k) <$$> spaces <**> symbol   a <**> spaces
token''    a = (\_ k _ -> k) <$$> spaces <**> token    a <**> spaces
optional'' a = (\_ k _ -> k) <$$> spaces <**> optional a <**> spaces
satisfy''  a = (\_ k _ -> k) <$$> spaces <**> satisfy  a <**> spaces
```
## Ambiguidades 

A ambiguidade de espaços surge porque estamos a consumir espaços tanto no início quanto no fim do parsing. Isto é necessário para cobrir casos como "if   (     a ==    0 ) then    {" ou mesmo "int  i    =    0  ;", onde há muitos espaços ao redor de um padrão. Ao usar estes combinadores ocorrem alguns problemas. Quando um tenta consumir espaços no final do parsing, mas o parser seguinte também consome espaços no início. Isso gera situações ambíguas quando há espaços extras. Por exemplo, se colocarmos um espaço nessa situação surgem 2 situações possíveis; se colocarmos 2 espaços, isso gera 4 combinações, resultando num comportamento exponencial.


Tentamos usar com cuidado estes combinadores para evitar consumir espaços no mesmo síteo quando há dois parsers seguidos. Mesmo assim quando chamamos tentamos dar parsing a uma função fatorial temos 65 ambiguidades das quais 24 o parsing correu bem e são árvores de sintaxe corretas. Outra forma que usamos para tentar diminuir o impacto é pegar na primeira ocorrencia correta do parsing, tentamos que ele seja lazy e pare logo quando encontrar uma válida.

### Distribuições
Usamos um módulo com o monad das distribuições que é fornecido na cadeira de cálculo de programas.

Existem 2 notações para distribuições que o nosso parser aceita:

    "int i = D (normal ,[1,100,80,30])"
    Constrói uma distribuição normal com elementos que sejam colocados numa lista

    "int i = D (uniform,1,100)"
    Constrói uma distribuição uniform com os elementos entre 1 e 100, como se tivessemos a lista [1..100]


Operações possíveis , resto da divisão, divisão inteira



#FIXME
PROPRIEDADE DE UNPARSING não dá para dists... teria que mudar a implementação do show

## Implementação do Gerador
Os geradores foram implementados utilizando o monad de estado para manter o nome das variáveis disponíveis durante a geração de expressões.
O gerador de expressões inteiros (gei) e de caracteres (ges) constroem recursivamente expressões compostas de constantes e operadores aritméticos, não inclui de boleanos porque se numa expressão númerica colcar um < menor, a expressão já não é do domínio dos inteiros mas dos boleanos. O gerador de expressões booleanas (geb) segue uma abordagem similar, mas inclui operadores booleanos. 
O gerador de atribuição (genAtrib) utiliza o estado de variáveis disponíveis com nomes gerados aleatoriamente e tipos definidos de forma arbitrária. Também gera uma expressão do tipo que sorteou. 
Essa abordagem permite a criação de programas variados e complexos para testar propriedades específicas usando o QuickCheck.

Também foi implementada uma função shrink para o PicoC.

## Implementação do Interpretador

Uma função de interpretação, `runP :: Programa -> Context -> IO Context`, foi desenvolvida para executar programas escritos na linguagem. Esta função executa as instruções do programa e mantém um contexto atualizado das variáveis criadas durante a execução.

Foram feitos 2 interpretadores para auxiliar a instrumentação de programas
* runDebug :: [Inst] -> Context -> IO Context, Este interpretador, imprime para o ecra o tipo de instrução que está a
  ser executado e devolve a memória como um normal.
* runL :: [Inst] -> Context -> [Inst] -> IO [Inst], Este devolve uma lista das intruções que foram executadas no programa

## Exemplos de Polimorfismo


A primeira tentativa foi usar o tipo Either mas não é adequado devido à sua limitação em aplicar funções diretamente a valores encapsulados, Como Either é um funtor não é possivel aplicar funções a ambos Left e Right, seria um bifuntor se fosse possível.
```
ghci> fmap (+3) (Left 4)
Left 4
ghci> liftM2 (+) (Left 179) (Left 12345)
Left 179
```

Por isso, criamos um tipo Out que representa o nosso output no PicoC que é um trifuntor com os tipos:
    * String
    * Integer
    * Bool

Implementamos um trimfmap para que fosse possível aplicar funções sobre Out e também implementar polimorfismo sobre algumas funções básicas. Quando na árvore de parsing há diferentes valores posso aplicamos funções diferentes. O comportamento polimorfico da nossa função de avaliação é o seguinte:
> Neg 4 = -4
> Neg False = True
> "asd" + "ola" = "asd" ++ "ola"
> True  + False = True || False
>  3    + 8     = 3 + 8
>  3    * 8     = 3 * 8
> True  * True  = True && True

Por quando não há defenido uma função para um tipo, por exemplo a multiplicação de Strings usamos uma função
"identidade" de aridade 2, que na prática é uma função constante e retorna só o segundo elemento.

## Tranformações , mutações essas cenas
FIXME

## Code Smeels
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

Se temos os elementos neutros e absorventes da adição e a sua soma também teriamos que ter para os outros tipos implementados: String e Bool. O nosso código tornar-se-ia muito repetitivo. Também, estas "melhorias" parecem que não passam de uma função de avaliação para casos simples, decidimos que estas otimizações são demasiados destrutivas da árvore de parsing e por isso devem ser retiradas.




## Exemplo de Execução

Para correr o programa chamarmos a função run com a árvore dados e uma memória vazia
run dados [] e obtemos a memória final depois da execução.

```
dados :: Programa
dados = [
    Atrib "x" "int" $ D2 (uniform [1..10]),
    Atrib "y" "int" $ D2 (uniform [1..10]),
    Atrib "z" "int" $ Mult (Fetch "x") (Fetch "y")
    ]

```
O programa dados é um programa que resulta em multiplicar 2 valores da distribuição normal com os valores entre 1 a
10. O resultado de correr o program algumas vezes é:

```
ghci> run dados []
[("z",R 10),("y",R 5),("x",R 2)]
ghci>
ghci> run dados []
[("z",R 80),("y",R 8),("x",R 10)]
ghci> run dados []
[("z",R 18),("y",R 9),("x",R 2)]
ghci> run dados []
[("z",R 80),("y",R 10),("x",R 8)]
ghci> run dados []
```

Outro exemplo, considere o seguinte programa que calcula o fatorial de 15:

```c
int n = 15;
if (n == 0) {
    int fact = 1;
}
else {
    int i = 1;
    int fact = 1;
    while (i < n) {
        fact = i * fact;
        i = i + 1;
    }
}
```

Após a execução deste programa, o contexto resultante é:

```
IO [("i",R 16),("fact",R 1307674368000),("n",R 15)]
```

Onde o valor de `fact` é o resultado do cálculo do fatorial de 15.



-------------------------------------------------------------------------------

## Conclusão
Este relatório apresentou uma visão geral da linguagem de programação proposta e sua implementação de interpretação. A linguagem oferece recursos como estruturas de controle, manipulação de variáveis e operações polimórficas, enquanto o interpretador permite a execução de programas escritos nesta linguagem. Com essa base sólida, é possível explorar e expandir ainda mais as capacidades da linguagem e do interpretador.
