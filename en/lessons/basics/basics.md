---
version: 1.0.0
title: Basics
---

Getting started, basic data types, and basic operations.

{% include toc.html %}

## Getting Started

### Installing Haskell

The recommended installation instructions for each OS can be found at
[ghcup](https://www.haskell.org/ghcup/).

You can check the version of the Haskell compiler installed by running
`ghc --version`:
```shell
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version {{ site.ghc.version }}
```
### Trying Interactive Mode

Your installation should come with `ghci`, an interactive shell that allows you
to try out code in real time.

Follow this tutorial by running `ghci`:
```console?lang=haskell&prompt=ghci>,ghci|
GHCi, version {{ site.ghc.version }}: https://www.haskell.org/ghc/  :? for help
ghci>
```
To use the `Text` type in this tutorial, type in:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :set -XOverloadedStrings
ghci> import qualified Data.Text as T
ghci> import qualified Data.Text.IO as T
```

You can try out Haskell by typing in a few simple expressions:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 2 + 3
5
ghci> 2 + 3 == 5
True
ghci> T.length "The quick brown fox jumps over the lazy dog"
43
```

Perhaps you are able to figure out what some of these expressions mean.

## Basic Data Types

### Integers

Haskell has a bounded integer type called `Int`:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 196883 :: Int
196883
```
__Note__: `::` specifies the type being used.

Haskell also supports arbitrary precision integers with `Integer`:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 123456789101112131415
123456789101112131415
```
__Note__: `ghci` automatically defaults to `Integer` here.

### Floats

Haskell supports `Double`, a double-precision floating-point number.
At least one digit must come before the decimal point and Haskell supports
scientific `e` notation.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 3.14
3.14
ghci> .14

<interactive>:2:1: error: parse error on input ‘.’
ghci> 5e-10
5.0e-10
ghci> 2.7e10
2.7e10
```

### Booleans

Booleans in Haskell are represented with `True` and `False`:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> True
True
ghci> False
False
```

### Text

Haskell supports Unicode text. Text is wrapped in double quotes,
and can be printed with `T.putStrLn`:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> T.putStrLn "Hello"
Hello
ghci> T.putStrLn "jalapeño"
jalapeño
```

Special characters can be represented with numeric escapes or escape codes:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> T.putStrLn "'hi\""
'hi"
ghci> T.putStrLn "dis\njointed"
dis
jointed
ghci> T.putStrLn "\42"
*
```

You'll learn more about data types in [collections](../collections/) and
[functions](../functions/).

## Basic Operations

### Arithmetic

Haskell supports the basic operators: `+`, `-`, `*`, `/`, `**` and `^`.
Note that `/` and `**` never operate on integers,
whereas `^` only accepts positive integer powers.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 2 + 2
4
ghci> 2 - 1
1
ghci> 2 * 5
10
ghci> 10 / 5
2.0
ghci> 2 ** (-0.5)
0.7071067811865476
ghci> 1.5 ^ 2
2.25
ghci> 5 ^ 2
25
```

For integer division and integer modulus, use `div` and `mod`:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> -8 `div` 3
-2
ghci> 243 `mod` 5
3
ghci> 2 * (5 `div` 2) + (5 `mod` 2)
5
```

### Boolean

Haskell provides the boolean operators `||`, `&&`, and `not` that operate on
`Bool`. These operators do not evaluate the second term if possible.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> False || False
False
ghci> True || False
True

ghci> False && True
False
ghci> True && True
True

ghci> not True
False
ghci> not False
True
```

### Comparison

Haskell comes with comparison operators for ordering: `<=`, `>=`, `<`, and `>`.
Conversely, the operator `==` compares equality whilst `/=` compares inequality.
Values compared must be of the same type.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 1 > 2
False
ghci> 2 < 2
False
ghci> 5 >= 5
True

ghci> True == True
True
ghci> False /= True
True
```

### String Concatenation

String concatenation uses the `<>` operator:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> T.putStrLn ("abra" <> "cadabra")
abracadabra
```
