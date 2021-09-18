---
version: 1.0.0
title: Control Structures
---

Recall that Haskell functions are *definitions*, not lists of instructions

In imperative programming, control structures are used to decide which block of
code should be executed

In Haskell, they are used instead to decide how a certain value should be
defined

__Note__: Usually control structures are used directly in the definition of a 
function, but they can also be used in the definition of any intermediate
value inside a function (we will see examples of both)

Let's have a look at the control structures available in Haskell, and some
examples of how they work

{% include toc.html %}

## Extensions and Imports

You will need to add the following lines to the top of your Haskell source 
file (`.hs`) to be able to run these examples

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text ( Text )
```

## If Statements

Haskell provides a simple set of keywords for `if` statements, with only a 
single `else` condition

### Syntax

```
result = if (boolExpression) 
         then (expression1)
         else (expression2)
```

- If the first expression evaluates to `True`, then the expression after the
  `then` keyword is evaluated
- if it evaluates to `False`, then the expression after the `else` keyword is
  evaluated

### Examples

```haskell
-- Is this integer even or odd?
evenOrOdd :: Int -> Text
evenOrOdd val = 
    if val `mod` 2 == 0 then "Value is even" else "Value is odd"

-- How to navigate a traffic light
trafficLight :: Text -> Text
trafficLight lightColor = 
    if (lightColor == "Red" || lightColor == "Yellow")
    then "Slow down"
    else "Roll through"

-- Give a nice description for the weather
-- Note how `warmOrCold` and `niceOrGloomy` are defined in a let block then 
-- used in the final definition
weatherDescription :: Text -> Int -> Text
weatherDescription weather tempInC = let
    warmOrCold   = if tempInC > 21       then "It's warm" else "It's cold"
    niceOrGloomy = if weather == "sunny" then "nice out"  else "gloomy out"
    in
    warmOrCold <> " and " <> niceOrGloomy
```

### Usage

`if` statements are not the most common control structure 

They are handy if you have a simple definition, and don't need one of the more
flexible structures we will see soon

## Case Statements

Case statements offer a way to switch on any one of a number of possible values

### Syntax

```
result = case (expression) of
    (pattern1) -> (expression1) 
    (pattern2) -> (expression2)
    ...
    _          -> (expressionLast)
```

- The expression between the `case` and `of` keywords is evaluated
- Its result is looked up in the list of patterns
- For the first pattern that matches, its expression is evaluated
- The underscore (`_`) is a catch-all pattern that will always match

### Examples

```haskell
-- Offer some helpful advice, given the weather
weatherAdvice :: Text -> Text
weatherAdvice weatherDescription = 
    case weatherDescription of
        "fair"     -> "It'll be nice today"
        "rainy"    -> "Bring an umbrella"
        "sunny"    -> "Wear sunscreen"
        "freezing" -> "You'll need a coat"
        _          -> "Not sure"
```

### Usage

Case statements are used commonly for intermediate values, and to decide the path
that code will take

The *Pattern Matching* feature they use will be discussed more in the next
lesson

## Guards

Guards allow you to choose between one of many possible conditions; they are
similar to an `if`-`else if` structure

### Syntax

```
result
    | (boolExpression1) = (resultExpression1)
    | (boolExpression2) = (resultExpression2)
    | ...
    | otherwise = (resultExpressionLast)
```

- Each expression on the right side of the `|` character evaluates to a `Bool`
- For the first one which evaluates to `True`, its expression is evaluated to
  define the result
- Note how the `=` symbol is not used between the result and the guards

### Examples

```haskell
-- State which value is bigger
biggerOrSmaller :: Int -> Int -> Text
biggerOrSmaller val1 val2
    | val1 > val2 = "Value 1 is bigger"
    | val2 > val1 = "Value 2 is bigger"
    | otherwise   = "Both values are equal"

-- Subjective descriptions of temperature, after converting to Celcius
-- Note how `tmpInC` is defined in the `where` clause
subjectiveTemp :: Double -> Text
subjectiveTemp tmpInF
    | tmpInC <= 0.0                   = "Freezing"
    | tmpInC > 0.0  && tmpInC <= 20.0 = "Cold"
    | tmpInC > 20.0 && tmpInC <= 30.0 = "Comfortable"
    | tmpInC > 30.0                   = "Hot"
    | otherwise                       = "Not Sure"
  where
    tmpInC = (tmpInF - 32.0) * ( 5.0 / 9.0 )

-- Clothing Sizer
-- Note that `yourSize` is defined in a where clause, then used in the
-- definition
whatSize :: Int -> Text
whatSize heightInCm = "You're a size " <> yourSize
  where
    yourSize
        | heightInCm > 0   && heightInCm <= 166 = "Small"
        | heightInCm > 166 && heightInCm <= 178 = "Medium"
        | heightInCm > 178                      = "Large"
        | otherwise                             = "Not sure"
```

### Usage

Guards are commonly used directly in function definitions, especially with
`where` statements

### Otherwise

`otherwise` is an alias for `True`, and is should be used as a catch-all for the
last guard, to ensure that one of them matches

If a guard statement is evaluated, and none of its expressions return `True`, a
runtime error occurs!

#### What Happens When You Don't Use `otherwise`

```haskell
badGuard :: Text -> Text
badGuard txtIn
    | txtIn == "Ok" = "It worked"
```

```console?lang=haskell&prompt=ghci>,ghci|
*Main> badGuard "noMatch"
"*** Exception: SchoolExamples.hs:(74,1)-(75,33): Non-exhaustive patterns in function badGuard
```

#### Back to Safety

```haskell
safeGuard :: Text -> Text
safeGuard txtIn
    | txtIn == "Ok" = "It worked"
    | otherwise = "It still worked"
```

```console?lang=haskell&prompt=ghci>,ghci|
*Main> safeGuard "noMatch"
"It still worked"
```

#### Catching Non-Exhaustive Guards

If you compile with `-Wall` or use `:set -Wall` in GHCi, the compiler will give
you a warning about non-exhaustive guards

```console?lang=haskell&prompt=ghci>,ghci|
*Main> :set -Wall
*Main> :r
[1 of 1] Compiling Main             ( SchoolExamples.hs, interpreted )

SchoolExamples.hs:74:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘badGuard’: Patterns not matched: _
   |
74 | badGuard txtIn
   | ^^^^^^^^^^^^^^...
```
