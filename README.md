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
FIXME

## Implementação do Interpretador

Uma função de interpretação, `runP :: Programa -> Context -> IO Context`, foi desenvolvida para executar programas escritos na linguagem. Esta função executa as instruções do programa e mantém um contexto atualizado das variáveis criadas durante a execução.

## Exemplos de Polimorfismo

Criamos um tipo Out que representa o nosso output no PicoC que é um trifuntor:
    * String
    * Integer
    * Bool

Implementamos um trimfmap para que fosse possível implementar polimorfismo sobre algumas funções básicas do tipo do output. Quando na árvore de parsing há diferentes valores posso aplicar funções diferentes. O comportamento polimorfico da nossa função de avaliação é o seguinte:
> Neg 4 = -4
> Neg False = True
> "asd" + "ola" = "asd" ++ "ola"
> True  + False = True || False
>  3    + 8     = 3 + 8
>  3    * 8     = 3 * 8
> True  * True  = True && True

Por quando não há defenido uma função para um tipo por
exemplo a multiplicação de Strings usamos uma função
"identidade" de aridade 2, é uma função constante que
retorna só o segundo elemento.

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

Se temos os elementos neutros e absorventes da adição e a sua soma também teriamos que ter para os outros tipos implementados: String e Bool. O nosso código tornar-se-ia muito repetitivo. Também, estas "melhorias" parecem que não passão de uma função de avaliação para casos simples, decidimos que estas otimizações são demasiados destrutivas da árvore de parsing e por isso devem ser retiradas.




## Exemplo de Execução

Para correr o programa chamarmos a função run com a árvore dados e uma memória vaiza
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
