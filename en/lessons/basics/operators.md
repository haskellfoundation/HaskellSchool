---
version: 1.0.0
title: Operators
---

Haskell provides constructs for using infix binary functions, called operators. In
this lesson you will learn how to define operators and control the parsing of
operators, and how infix and prefix syntax can be interchanged.

{% include toc.html %}


## Using Operators

Operators in Haskell are functions that can be placed between two arguments.
There are two cases where you can use a function as an operator: when the function name
only consists of symbols or when you surround the function name in backticks.
Using infix notation makes it easier to write expressions containing many common functions,
and this article will help you in understanding and writing such expressions: 

```console?lang=haskell&prompt=ghci>,ghci|
ghci> abs . subtract 119 $ 2 + 2
115
ghci> (`mod` 930233356) . negate $ 4
930233352
ghci> - (7 + 57843)
-57850
```

You may have seen operators such as `+`, `*`, and `.` before. The `$` operator is
useful for removing parentheses in function application: `abs . subtract 1 $ 2 + 2` is
equivalent to `(abs . subtract 1) (2 + 2)`. You can find more examples of operators on
[FPComplete's Operator Glossary](https://www.fpcomplete.com/haskell/tutorial/operators/).

### Interchanging Operators and Functions

To use an operator symbol as if it were a regular function, surround it in parentheses.
Furthermore, you can create partially applied operators, called sections, by putting
an expression on only one side of the operator:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> (+) 2 3
5
ghci> (5 <=) . (* 2) $ 3
True
```
__Note__: negate (`-`) is the only unary operator in Haskell. This can make sectioning
the subtraction operator, which uses the same symbol, difficult.

The parenthesised operators in the above example can be treated as equivalent to the following
anonymous functions:
```console?lang=haskell&prompt=ghci>,ghci|
ghci> (\a b -> a + b) 2 3
5
ghci> (\a -> 5 <= a) . (\a -> a * 2) $ 8
True
```

Conversely, to make a regular function identifier an operator, surround it in backticks:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> mod 2010 1998
12
ghci> 2010 `mod` 1998
12
ghci> (`div` 3) . (* 1471) $ 5
2451
```


### Operator Precedence and Associativity

An operator's precedence and associativity combined is called its fixity, and it
controls how the operator is parsed in an expression. To find out the fixity of an
operator, look through `ghci`'s `:info` or the documentation for the operator:
if fixity information isn't displayed, it defaults to `infixl 9`. The same applies to
regular functions used in infix mode.
```console?lang=haskell&prompt=ghci>,ghci|
ghci> :info (+)
infixl 6 +
ghci> :i ($)
infixr 0 $
ghci> :i (==)
infix 4 ==
ghci> :i elem
infix 4 `elem`
```

In Haskell, operators have a precedence level between 0 and 9: this precedence level
is the number in the operator's fixity. To see how a group of expressions
separated by operators would be parenthesised, surround the highest precedence operator
and the expressions next to it in parentheses, and repeat this process until there
is only one operator left at the highest level of the expression. Here's an example of
applying this process:
```
16425973 + 17149685 * (106 + 235) == 3530784511
         |          │              ╰─ Precedence level 4
         |          ╰─ Precedence level 7
         ╰─ Precedence level 6
16425973 + (17149685 * (106 + 235)) == 3530784511
(16425973 + (17149685 * (106 + 235))) == 3530784511
```

Knowing operator precedence is helpful in writing code without unnecessary parentheses:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> (^ (3 - 1)) . subtract 3818 $ 4
14546596
ghci> (1729 + 1) * 1 `div` 2
865
ghci> 1 + 6 <= 7952317874 + 6 && 0 == 0
True
```

However, this process may be ambiguous: multiple operators of the highest precedence
could exist. Operator associativity solves this problem. An operator has either left
(`infixl`), right (`infixr`), or neutral (`infix`) associativity. If either all the
operators of the highest precedence in an expression have left associativity, or they
all have right associativity, you can parenthesise the expression by selecting
the leftmost or rightmost operator of highest precedence accordingly. For example,
given that both `+` and `-` have left associativity, you could do the following
process to parenthesise an expression:
```
196418 - 514229 + 317811 - 5605
(196418 - 514229) + 317811 - 5605
((196418 - 514229) + 317811) - 5605
```

Otherwise, if multiple operators of the highest precedence exist, and either one has
neutral associativity or they differ in associativity, Haskell will throw an error:
```console?lang=haskell&prompt=ghci>,ghci|
ghci> 29 + 61 == 3 == True
<interactive>:1:1: error:
    Precedence parsing error
        cannot mix ‘==’ [infix 4] and ‘==’ [infix 4] in the same infix expression
ghci> 0 <> 1 + (-1)
<interactive>:2:1: error:
    Precedence parsing error
        cannot mix ‘<>’ [infixr 6] and ‘+’ [infixl 6] in the same infix expression
```

Operator associativity is not chosen arbitrarily: for example, `1 - 6 - 2` equals `-7`,
whereas `1 - (6 - 2)` equals `-3`. Similarly, the expression `not $ not $ True` returns `True`, but
`(not $ not) $ True` doesn't even type-check. On a more advanced note, using certain associativities
is sometimes faster: because appending to regular lists in Haskell is slow, the list concatenation
operator is right associative, which prevents the repeated appending of long lists.

## Defining Operators

An operator is just a function placed between two expressions, so they are defined
exactly the same way that functions are. Try to use operators mainly for the most
common functions in your code, and provide non-operator aliases, because
function names give stronger indications of what a function does.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> m |--> v = 0.5 * m * v^2
ghci> (f ... g) x = f (g x)
ghci> (f `on` g) a b = f (g a) (g b)
ghci> (&++) a b = a + b + 2
ghci> on' f g a b = f (g a) (g b)
```
__Note__: operator symbols beginning with `:` form binary constructors: you'll learn about this in [types](../types).

To define the fixity of an operator, put a fixity declaration next to the function
declaration; you can also define an operator's type signature in this manner. A fixity declaration
lacking a precedence level defaults to precedence level 9:
```console?lang=haskell&prompt=ghci>,ghci|
ghci> :{
ghci| infixl 5 ^*^
ghci| a ^*^ b = a * b
ghci| :}
ghci> 1342 + 2 ^*^ 5
6720
ghci> :{
ghci| infixr >-<
ghci| (>-<) :: Integer -> Integer -> Integer
ghci| a >-< b = b - a
ghci| :}
ghci> 99 >-< 81 >-< 90
-90
ghci> :{
ghci| infix 4 `roundsTo`
ghci| a `roundsTo` b = round a == b
ghci> :}
ghci> pi `roundsTo` 3
True
```

__Note__: in `ghci`, `:{` and `:}` allow you to type definitions spanning multiple lines.
