---
version: 1.0.0
title: Types
---

Types are a fundamental and integral part of Haskell. Haskell's advanced 
type system sets it apart from other languages, and is a powerful method for
ensuring that your code does what you want it to do.

{% include toc.html %}

## Extensions and Imports

You will need to add the following lines to the top of your Haskell source 
file (`.hs`) to be able to run these examples:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text ( Text )
```

## Static Typing

Haskell is a statically-typed programming language. This means that the type
of all data must be known before the program is compiled. Some imperative 
languages, such as Java and C++ are also statically-typed, while others like 
Python and JavaScript are not.

## Type Inference

Haskell's type system works very differently from that of Java and C++. 
Instead of declaring the type of every single function and variable, Haskell
can *infer* the type of values based on just a few declarations.

Typically, you just need to specify the type of a function. The types of all
intermediate values used in the function will be inferred by Haskell, and you
don't have to state their types explicitly.

Type inference is a killer feature of Haskell, and once you get used to it, you
won't want to work without it.

## Type Declarations

In Haskell, you declare types on a separate line from where they are used. 
Even though I said above that you usually only declare types for functions, in
these examples we are going to declare them for single values.

Type names are always capitalized in Haskell, and value (or function) names 
are always lower-case.

### Syntax

```
valueName :: Type
valueName = definition
```

## Primitive types

### Boolean

Haskell has booleans, and they can either be `True` or `False`

```haskell
imTrue :: Bool
imTrue = True

imFalse :: Bool
imFalse = False
```

### Integer

Haskell has two common integer types, `Int` and `Integer`

#### `Int`

A fixed-precision type, with at least the range `[-2^29 .. 2^29-1]`, or 30 bits
It is commonly implemented as 32 or 64 bits

```haskell
positiveInt :: Int
positiveInt = 42

negativeInt :: Int
negativeInt = -37
```

The fact that `Int`'s size is limited means it is subject to overflows. These 
occur without warning, so if they are a concern, use `Integer`

#### `Integer`

An arbitrary-precision type. Its size is only limited by the memory on your
machine.

```haskell
positiveInteger :: Integer
positiveInteger = 42

negativeInteger :: Integer
negativeInteger = -37
```

### Floating Point

Haskell has floating-point numbers, single-precision `Float` and 
double-precision `Double`

A `Float` will be 32 bits. For more information see this [wikipedia page](https://en.wikipedia.org/wiki/Single-precision_floating-point_format)
A `Double` will be 64 bits. For more information see this [wikipedia page](https://en.wikipedia.org/wiki/Double-precision_floating-point_format)

```haskell
aFloat :: Float
aFloat = 3.14159

smallDouble :: Double
smallDouble = 0.00003

mediumDouble :: Double
mediumDouble = 17.4

bigDouble :: Double
bigDouble = 156042.4039

negativeDouble :: Double
negativeDouble = -137.485

wholeDouble :: Double
wholeDouble = 42.0
```

### Char

Haskell represents single characters in the `Char` type.

`Char` literals are always surrounded with single quotes (`''`)

```haskell
imAChar :: Char
imAChar = 'a'

imZChar :: Char
imZChar = 'z'

singleQuote :: Char
singleQuote = '\''
```

### Text

Text values can go in the `Text` type

`Text` literals are always surrounded with double quotes (`""`)

```haskell
someText :: Text
someText = "Let's learn Haskell"
```

#### The `String` Type

Haskell's default text type is `String`. You may notice that none of the 
examples in this course refer to `String`, using `Text` instead.

Unfortunately, `String` is a bit of a historical accident. Internally, it 
is a linked-list of single characters. While this may be a satisfying use of
linked lists, it is not a practical design for representing strings.

Since it has been around from the beginning, and it works fine for small
strings, the `String` type is common in Haskell. It is used everywhere in
Haskell's default libraries.

`Text` is more convenient, performant and capable than `String`. We want 
beginners to avoid the mistakes of the past and start off with good 
practices, so we have chosen to teach `Text`. 
