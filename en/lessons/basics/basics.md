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

    % ghc --version
    The Glorious Glasgow Haskell Compilation System, version {{ site.ghc.version }}

### Trying Interactive Mode

Your installation should come with `ghci`, an interactive shell that allows you
to try out code in real time.

Follow this tutorial by running `ghci`:

    GHCi, version {{ site.ghc.version }}: https://www.haskell.org/ghc/  :? for help
    ghci>

To use `Text` in Haskell, type in:

```haskell
ghci> :set -XOverloadedStrings
ghci> import qualified Data.Text as T
```

You can try out Haskell by typing in a few simple expressions:

```haskell
ghci> 2 + 3
5
ghci> 2 + 3 == 5
True
ghci> T.length "The quick brown fox jumps over the lazy dog"
43
```

Perhaps you're able to figure out what some of these expressions mean.

## Basic Data Types

### Integers

Haskell has a bounded integer type called `Int`:

```haskell
ghci> 255 :: Int
255
```
__Note__: `::` specifies the type being used.

Haskell also supports arbitrary precision integers with `Integer`:

```haskell
ghci> 123456789101112131415
123456789101112131415
```
__Note__: `ghci` automatically defaults to `Integer` here.

### Floats

Haskell supports `Double`, a double-precision floating-point number.
At least one digit must come before the decimal point and Haskell supports
scientific `e` notation.

```haskell
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

```haskell
ghci> True
True
ghci> False
False
```

### Text

Haskell supports unicode text. Strings are wrapped in double quotes:

```haskell
ghci> "Hello" :: T.Text
"Hello"
ghci> "jalapeño" :: T.Text
"jalape\241o"
```

Special characters can be represented with numeric escapes or escape codes:

```haskell
ghci> "'hi\"" :: T.Text
"'hi\""
ghci> "\n" :: T.Text
"\n"
ghci> "\42" :: T.Text
"*"
```

You'll learn more about data types in [collections](../collections/) and
[functions](../functions/).

## Basic Operations

### Arithmetic

Haskell supports the basic operators `+`, `-`, `*`, and `/`.
Note that `/` never returns an integer.

```haskell
ghci> 2 + 2
4
ghci> 2 - 1
1
ghci> 2 * 5
10
ghci> 10 / 5
2.0
```

For integer division and integer modulus, use `div` and `mod`:

```haskell
ghci> -8 `div` 3
-2
ghci> 243 `mod` 5
3
ghci> 2 * (5 `div` 2) + (5 `mod` 2)
5
```

### Boolean

Haskell provides the boolean operators `||`, `&&`, and `not` that operate on
`Bool`:

```haskell
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
The operator `==` compares equality whilst `/=` compares inequality.
Values compared must be of the same type.

```haskell
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

```haskell
ghci> "abra" <> "cadabra" :: T.Text
"abracadabra"
```
