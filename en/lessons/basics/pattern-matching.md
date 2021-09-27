---
version: 1.0.0
title: Pattern Matching
---

Haskell offers a powerful type-based system for defining functions called 
*pattern matching*

In general, pattern matching is used to deconstruct algebraic types

We will look at an example of how it works, talk about it in general, then see
some more examples

{% include toc.html %}

## Extensions and Imports

You will need to add the following lines to the top of your Haskell source 
file (`.hs`) to be able to run these examples

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text ( Text )
```

## Enumerated Type Example

Say we have a type for the four suits of playing cards

```haskell
data Suit = Hearts | Diamonds | Clubs | Spades
```

### Using a Case Statement

Let's write a case statement which will read the color of the suit

```haskell
-- Show the color of the suit
suitColorCase :: Suit -> Text
suitColorCase suitIn = case suitIn of
     Hearts   -> "Red"
     Diamonds -> "Red"
     Clubs    -> "Black"
     Spades   -> "Black"
```

The case statement is using pattern matching to decide which text to return

The value of `suitIn` is evaluated, and then checked against the given patterns
in the case statement
- In this example, each pattern is just a data constructor
- We will get to see more sophisticated patterns a bit later

### Using Pattern Matching

It turns out that we can write the same function more simply

```haskell
-- Show the color of the suit
suitColor :: Suit -> Text
suitColor Hearts   = "Red"
suitColor Diamonds = "Red"
suitColor Clubs    = "Black"
suitColor Spades   = "Black"
```

Here we have built the patterns into the function definition, without using a
case statement

The function works the same way: any input value is inspected and matched
with the patterns provided for the function definition

In truth, the case statement is redundant in the first example, and as
Haskell programmers we like to keep our code as concise as is reasonable

This can feel awkward at first, since in imperative languages there is usually
only a single declaration of a function
- In Haskell, pattern matching syntax is common
- Always keep in mind that Haskell functions are definitions, and with pattern
  matching we are providing different definitions for different inputs

## Pattern Matching Syntax

```
functionName (pattern1) = (expression1)
functionName (pattern2) = (expression2)
...
functionName _ = (expressionLast)
```

- A function is defined more than one time, each with a different pattern
- For the first pattern that matches the value with which the function was
  called, its expression is evaluated
- The underscore (`_`) is a catch-all that matches any value

### Patterns In General

A pattern is a combination of data constructors that could match a value

Functions are not permitted in patterns -- If you need to use a function,
consider using guards

#### Examples of Patterns

```
"banana" -- A `Text` value
7        -- An `Int` value
[]       -- An empty list
(x:xs)   -- A list with one or more values, constructed with `:`
         -- `x` is the first element of the list, and `xs` is the rest of the list
Nothing  -- A constructor for the `Maybe` type
(Just v) -- A constsructor for the `Maybe` type, where `v` is the value
```

#### Examples of Non-Patterns

```
(7 - 2)     -- Functions are not allowed in patterns: `-` is a function
(x > 5)     -- Functions are not allowed in patterns: `>` is a function
            -- Consider a guard
(=="apple") -- Seriously, no functions allowed: `==` is a function
```

## List Example

Using pattern matching to handle lists is very common. Let's see some examples

### First Try

```haskell
-- How big is a list?
listSize1 :: [Text] -> Text
listSize1 []      = "Empty"
listSize1 [elem1] = "One element: " <> elem1
```

This is a dangerous function! Let's discover why
- The first pattern `[]` will match an empty list
- The second pattern `[elem1]` will match a single-element list
- What happens the list has more than one element?

A runtime error happens! This is an appalling to a Haskell developer; the whole
point of using Haskell is to catch issues like this at compile time

### A Safer Choice

Here is a safe example of list pattern matching

```haskell
-- How big is a list?
-- Using the `_` pattern is a catch-all for any value
listSizeCatch :: [Text] -> Text
listSizeCatch []      = "Empty"
listSizeCatch [elem1] = "One element: " <> elem1
listSizeCatch _       = "More than one element"
```

The `_` pattern matches everything, so we know this function will work no matter
what the first value is

#### Catching Non-Exhaustive Patterns

If you compile with `-Wall` or use `:set -Wall` in GHCi, you will be warned 
about non-exhaustive patterns:

```console?lang=haskell&prompt=ghci>,ghci|
    Pattern match(es) are non-exhaustive
    In an equation for ‘listSize1’: Patterns not matched: (_:_:_)
   |
92 | listSize1 [] = "Empty"
   | ^^^^^^^^^^^^^^^^^^^^^^...
```

This is why `-Wall` is recommended; nobody wants a nasty surprise when they run
their code

### Matching With Constructors

We can use any data constructor in patterns, including the list constructor `:`

```haskell
-- How big is a list?
listSizeCons :: [Text] -> Text
listSizeCons []         = "Empty"
listSizeCons (elem1:[]) = "One element: " <> elem1
listSizeCons (elem1:_)  = "More than one element, starting with: " <> elem1
```

Notice how the list constructor `:` is used in the patterns
- In the `(elem1:[])` pattern, it constructs the value with an empty list `[]`
  - This will only match a single-element list
  - It is the same as writing `[elem1]`
- In the `(elem1:_)` pattern, it constructs the value with the catch-all
  symbol `_`
  - So this pattern will match any list with one or more element
  - This pattern would match a single-element list, but it won't since it is
    listed *after* single-element pattern

Order matters! The first matching pattern will be used, even if later patterns
would also match

__Note__: Parentheses `()` are required to write patterns with constructors

__Note__: We do not need the final catch-all symbol `_`, because we have covered
lists of every possible size: empty, one element, more than one element

## Maybe

### Pattern Matching in Function Definition

Pattern matching provides a nice way to deconstruct `Maybe` values

```haskell
maybeToText :: Maybe Text -> Text
maybeToText Nothing  = "No text! Nothing!"
maybeToText (Just t) = "The text is: " <> t
```

Notice how the value `t` can be used if the `(Just t)` pattern is matched; of
course this value doesn't exist if the `Maybe` value is `Nothing`

As with lists, the parentheses `()` are necessary when we are matching the
`Just` constructor

### Pattern Matching in an Intermediate Value

Let's use pattern matching in a case statement to define an intermediate value

```haskell
maybeWithCase :: Maybe Text -> Text
maybeWithCase mText = let
    textFound = case mText of
        Nothing -> "No text"
        (Just t) -> t
    in
    "I found: " <> textFound
```

## Either

Pattern matching is common for deconstructing Either values

```haskell
eitherToText :: Either Text Text -> Text
eitherToText (Left errMsg) = "Error: " <> errMsg
eitherToText (Right okMsg) = "Success: " <> okMsg

eitherWithCase :: Either Text Text -> Text
eitherWithCase eText = "I got a " <> result
  where
    result = case eText of
        (Left errMsg) -> "Error: " <> errMsg
        (Right okMsg) -> "Success: " <> okMsg
```

### Pattern Matching With Multiple Arguments

```haskell
eitherMulti :: Either Text Text -> Text -> Text
eitherMulti (Left errMsg) _ = "Error: " <> errMsg
eitherMulti (Right okMsg) intro = intro <> okMsg
```

Notice how the catch-all symbol `_` is used in the first pattern, because we do
not need that argument

