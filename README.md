# Relatório de Linguagem e Implementação de Interpretação

### João Afonso Alvim Oliveira Dias de Almeida - pg53902@alunos.uminho.pt
### Simão Oliveira Alvim Barroso - pg54236@alunos.uminho.pt
### Simão Pedro Cunha Matos - pg54239@alunos.uminho.pt

## Introdução

Este relatório descreve uma linguagem de programação e sua implementação de interpretação. A linguagem apresentada possui uma sintaxe definida e suporta várias características, incluindo estruturas de controle condicionais, loops, manipulação de variáveis e distribuições de valores. Além disso, o relatório detalha a implementação de um interpretador para executar programas escritos nesta linguagem. Também um gerador FIXME

## Definição da Linguagem

A linguagem é definida por meio de uma estrutura de dados em Haskell. A seguir, estão os principais componentes da linguagem:

### Tipos de Dados
- `PicoC`: Representa a nossa linguagem completa, é o nosso construtor root da linguagem

- `Exp`: Representa expressões na linguagem, incluindo constantes, operações aritméticas, operações lógicas e operações de comparação.

- `Inst`: Define instruções executáveis, como estruturas condicionais (`IfElse`), loops (`While`), atribuição de valores (`Atrib`) e pausa (`Wait`).

- `Out` : Denine o nosso tipo de valores permitidos na linguagem. 

### Características da Linguagem

- **Construtor If-Else Opcional**: A estrutura `IfElse` não exige um ramo `else`, tornando-o opcional para maior flexibilidade na codificação.

- **Distribuições de Valores**: Introduz o conceito de distribuições de inteiros (`Dist Int`), permitindo a avaliação de elementos com base em valores aleatórios que seguem uma distribuição específica.

- **Instrução Wait**: O construtor `Wait` permite esperar a execução do programa por um número especificado de segundos, utilizando a função `threadDelay :: Int -> IO ()`.

- **Output Polimórfico**: Define um tipo `Out` que representa um valor da linguagem, permitindo valores de tipo `String`, `Integer` e `Bool`. Isso possibilita uma saída flexível e adaptável às necessidades do programa.

- **Intrução Print**: Introduz a capacidade de imprimir coisas no ecra.
- **Polimorfismo**: Implementa funções polimórficas básicas, como adição e multiplicação, para lidar com diferentes tipos de entrada de forma coerente.

## Implementação do Parser
### Gramática da linguagem
Esta defenição é informal, usamos FIXME colcoar na notação Backus–Naur form

```
Linhas -> Ordem+

Ordem  -> Atribuicao
        | While
        | IfElse 
        | Comentario
        | Print
        
IfElse -> 'if' '(' Expressao ')' 'then' '{' Linhas '}' ('else' '{' Linhas '}')?

While -> 'while' '(' Expressao ')' '{' Linhas '}' 

Atribuicao -> Tipo? Davalor ';'

Davalor -> pal ( '=' Expressao )?

Tipo -> 'char' | 'int' | 'bool'

Print -> 'print' '(' Expressao ')'

Comentario -> '//' .* '//'

Expressao -> Exp1 '>'  Expressao 
           | Exp1 '<'  Expressao 
           | Exp1 '==' Expressao 
                   ||
Epx1 -> Exp2  '+' Expressao
      | Exp2  '-' Expressao
Epx2 -> Fator '%' Expressao  
      | Fator '*' Expressao 
      | Fator '/' Expressao 
Fator -> Valor
       | '(' Expressao ')'

Valor -> Distribuicao
       | int 
       | string
       | bool
       | var

Distribuicao -> 'D' '(' pal ',' int ',' int ')'
              | 'D' '(' pal ',' Lista ')'

Lista ->  '[' separatedBy (int  ',')']'
```


No parser em vez de usarmos o combinador \<\$\> e o \<\*\> usamos um igual mas com outro nome, o \<\$\$\> e o \<\*\*\> para evitar
conflitos e não se confundir com o map sobre funtores e o map sobre funtores aplicativos.

### Novos combinadores de parsing 
Tivemos que definir novos combinadores, surgiram algumas dificuldades por exemplo: quando se tenta fazer um parser para consumir várias palavras seguidas temos a seguinte defenição:

```
palavras = zeroOrMore (satisfy' isAlphaNum)
```

```
palavras "a b" = [("","a b"),("a"," b"),("a","b"),("ab","")]
```
Não funciona porque consome uma string com espaços e devolve as palavras concatenadas sem os espaços, porque o operador ``` satisfy' ``` consome espaços no fim. Se antes usarmos o ``` satisfy ``` sem a plica temos que:

```
palavras "a b" = [("","a b"),("a"," b")]
```

neste caso consome uma palavra mas o resto da string tem espaços no início linha logo o próximo parser terá que conseguir consumi-los.

Por isso é útil consumir espaços não só fim como o no início. Surge a necessidade de criar novos combinadores:

```
symbol''   a = (\_ k _ -> k) <$$> spaces <**> symbol   a <**> spaces
token''    a = (\_ k _ -> k) <$$> spaces <**> token    a <**> spaces
optional'' a = (\_ k _ -> k) <$$> spaces <**> optional a <**> spaces
satisfy''  a = (\_ k _ -> k) <$$> spaces <**> satisfy  a <**> spaces
```
## Ambiguidades 

A ambiguidade de espaços surge porque estamos a consumir espaços tanto no início quanto no fim do parsing. Isto é necessário para cobrir casos como `"if   (     a ==    0 ) then    {"` ou mesmo `"int  i    =    0  ;"`, onde há muitos espaços ao redor de um padrão. Ao usar estes combinadores ocorrem alguns problemas. Quando um tenta consumir espaços no final do parsing, mas o parser seguinte também consome espaços no início. Isso gera situações ambíguas quando há espaços extras. Por exemplo, se colocarmos um espaço nessa situação surgem 2 situações possíveis; se colocarmos 2 espaços, isso gera 4 combinações, resultando num comportamento exponencial.


Tentamos usar com cuidado estes combinadores para evitar consumir espaços no mesmo síteo quando há dois parsers seguidos. Mesmo assim quando chamamos tentamos dar parsing a uma função fatorial temos 24 ambiguidades onde o parsing correu bem e são árvores de sintaxe corretas. Para diminuir o impacto tentamos pegar na primeira ocorrencia correta do parsing, para que ele seja lazy e pare logo quando encontrar uma válida.

### Exp Expressões 
Tipicamente a negação lógica tem baixa prioridade mas como esta negação é polimórfica também é numérica por isso atribuímos muita prioridade.

FIXME FALAR DE CENAS
Nível de prioridade mais baixo:
* Igualdade - "=="
* Or - "||"
* Maior - ">"
* Menor - "<"

Nível 2 de prioridade:
* Soma - "+"
* Subtração - "-"

Nível 3 de prioridade:
* Divisão inteira - "/"
* Resto da divisão - "%"
* Multiplicação - "\*"
* Negação - "~"

Nível com mais prioridade:
* Parentesis - "(" e ")"


### Distribuições
Usamos um módulo com o monad das distribuições que é fornecido na cadeira de cálculo de programas.

Existem 2 notações para distribuições que o nosso parser aceita:

    "int i = D (normal ,[1,100,80,30])"
Constrói uma distribuição normal com elementos que sejam colocados numa lista

    "int i = D (uniform,1,100)"
Constrói uma distribuição uniform com os elementos entre 1 e 100, como se tivessemos a lista [1..100]

Na semantica implementada, quando uma variável pertence a uma distribuição esta fica com um valor aleatório cumprindo a distribuição. Pode ser útil se quisermos simular o lançamento de dados.

## Implementação do Gerador

Os geradores foram implementados utilizando o monad de estado para manter o nome das variáveis disponíveis durante a geração de expressões. A ideia é se utilizar variáveis só utilizar as que já tenham sido atribuidas.

O gerador de expressões inteiros `gei` e de caracteres `ges` constroem recursivamente expressões compostas de constantes e operadores aritméticos, não inclui de boleanos porque se numa expressão númerica colcar um < menor, a expressão já não é do domínio dos inteiros mas dos boleanos. O gerador de expressões booleanas `geb` segue uma abordagem similar, mas inclui operadores booleanos. Na geração de expressoes inteira é possível também gerar distribuições neste momento está limitada a gerar distribuições normais ou uniformes

O gerador de atribuição `genAtrib` atualiza o estado de variáveis disponíveis com nomes gerados aleatoriamente e tipos definidos de forma arbitrária. Também gera uma expressão do tipo que sorteou. 

Também foi implementada uma função shrink para o PicoC.

## Implementação do Interpretador

Uma função de interpretação, `runP :: Programa -> Context -> IO Context`, foi desenvolvida para executar programas escritos na linguagem. Esta função executa as instruções do programa e mantém um contexto atualizado das variáveis criadas durante a execução.

Foram feitos 2 interpretadores para auxiliar a instrumentação de programas
* runDebug :: [Inst] -> Context -> IO Context, Este interpretador, imprime para o ecra o tipo de instrução que está a
  ser executado e devolve a memória como um normal.
* runL :: [Inst] -> Context -> [Inst] -> IO [Inst], Este devolve uma lista das intruções que foram executadas no programa

## Exemplos de Polimorfismo


A primeira tentativa de criat um tipo output foi usar o tipo Either mas não é adequado devido à sua limitação em aplicar funções diretamente a valores encapsulados. Como Either é um funtor não é possivel aplicar funções a ambos Left e Right, seria um bifuntor se fosse possível.
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

```
Neg 4 = -4
Neg False = True
"asd" + "ola" = "asd" ++ "ola"
True  + False = True || False
 3    + 8     = 3 + 8
 3    * 8     = 3 * 8
True  * True  = True && True
```

Quando não há defenido uma função para um tipo, por exemplo a multiplicação de Strings usamos uma função
"identidade" de aridade 2, que na prática é uma função constante e retorna só o segundo elemento. Isso significa que
frases como ' "ola" % "tudobem?" ' são frases válidas na linguagem, neste caso iria ser avaliado para "tudobem?".

## Transformações

### Mutações 
A função que usamos para mutar um PicoC injeta apena 1 mutação de 1 dos tipos possíveis:
* mutações nas intruções 
* mutações nas expressões
FIXME 

### Otimizações e Code Smeels
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
```
Add a + 0 = a
Add a + 1 = a
Mult a * 0 = 0
Mult a 1 = a
Add a b = a + b
```

Se o nosso programa fosse ser compilado em vez de interpretado, talvez fize-se sentido avaliar constantes sempre que elas aparecessem na
árvose de sintaxe. Como vai ser interpretado estas otimizações não passam de uma função de avaliação para casos simples, decidimos que estas otimizações são demasiados destrutivas da árvore de parsing e por isso devem ser retiradas. 

Se temos os elementos neutros e absorventes da adição e a sua soma também teriamos que ter para os outros tipos implementados: String e Bool. O nosso código tornar-se-ia muito repetitivo. 

## Propriedades da linguagem

Defenimos `pu` que é a abreviação de fazer unparsing e depois parsing.
```
pu = parser . unparse
```

Defenimos o operador `~`, que compara 2 PicoC semanticamente, corresponde a correr os dois e ver se o seu output é o
mesmo.
```
(~) :: PicoC -> PicoC -> IO Bool
(Pico p) ~ (Pico q) = liftM2 (==) (run p []) (run q [])
```


A **propriedade 1** compara as árvores, testa se quando fazemos unparse e parse obtemos uma equivalente árvore sintaxe abstrata.
```
propriedade1 pico = pico == pu pico
```


A **propriedade 2** compara a semântica da linguagem. Testa se quando fazemos unparse e parse de uma linguagem mesmo que diferentes, obtemos uma memória equivalente.
```
propriedade2 pico = pico ~ pu pico
```

Também temos uma terceira propriedade que combina as útimas 2, que testa se 2 PicoC são iguais semanticamente e se a arvore sintaxe abstrata são iguais.


As nossas propriedades funcionam sempre exceto quando usamos o monad das distribuições.
A razão de isso acontecer é que quando se compara o resultado de programas com valores aleatórios quase sempre dão diferentes e por isso não passa na propriedade.

Também não passa na propriedade que compara as árvores abstratas depois do uparsing e parsing. O problema está na
instancia Show das distribuições. Para cumprir a propriedade teríamos que alterar o Show das distribuições para que
fosse possível o parsing pelo nosso parser de PicoC.

Testes das propriedades com alguns exemplos:
FIXME falta colocar

## Funções de teste
FIXME


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
