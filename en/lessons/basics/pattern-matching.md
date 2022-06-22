---
version: 1.0.0
title: Pattern Matching
---

Haskell offers a powerful type-based system for defining functions called 
*pattern matching*.

In general, pattern matching is used to deconstruct algebraic types.

We will look at an example of how it works, talk about it in general, then see
some more examples.

{% include toc.html %}

## Extensions and Imports

You will need to add the following lines to the top of your Haskell source 
file (`.hs`) to be able to run these examples:

```haskell
{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}

import           Data.Text ( Text )
import qualified Data.Text as T
```

## Enumerated Type Example

Say we have a type for the four suits of playing cards:

```haskell
data Suit = Hearts | Diamonds | Clubs | Spades
```

### Using a Case Statement

Let's write a case statement which will read the color of the suit:

```haskell
-- Show the color of the suit
suitColorCase :: Suit -> Text
suitColorCase suitIn = case suitIn of
     Hearts   -> "Red"
     Diamonds -> "Red"
     Clubs    -> "Black"
     Spades   -> "Black"
```

The case statement is using pattern matching to decide which text to return.

The value of `suitIn` is evaluated, and then checked against the given patterns
in the case statement.
- In this example, each pattern is just a data constructor.
- We will get to see more sophisticated patterns a bit later.

### Using Pattern Matching

It turns out that we can write the same function more simply:

```haskell
-- Show the color of the suit
suitColor :: Suit -> Text
suitColor Hearts   = "Red"
suitColor Diamonds = "Red"
suitColor Clubs    = "Black"
suitColor Spades   = "Black"
```

Here we have built the patterns into the function definition, without using a
case statement.

The function works the same way: any input value is inspected and matched
with the patterns provided for the function definition.

In truth, the case statement is redundant in the first example, and as
Haskell programmers we like to keep our code as concise as is reasonable.

This can feel awkward at first, since in imperative languages there is usually
only a single declaration of a function.
- In Haskell, pattern matching syntax is common.
- Always keep in mind that Haskell functions are definitions, and with pattern
  matching we are providing different definitions for different inputs.

## Pattern Matching Syntax

```
functionName       (pattern1) = (expression1)
functionName       (pattern2) = (expression2)
functionName name3@(pattern3) = (expression3)
...
functionName _ = (expressionLast)
```

- A function is defined more than one time, each with a different pattern.
- For the first pattern that matches the value with which the function was
  called, its expression is evaluated.
- Entire patterns can be given local names by preceding them with the at (`@`)
  symbol.
- The underscore (`_`) is a catch-all that matches any value.

### Patterns In General

A pattern is a combination of data constructors that could match a value.

Functions are not permitted in patterns -- If you need to use a function,
consider using guards.

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

Using pattern matching to handle lists is very common. Let's see some examples.

### First Try

```haskell
-- How big is a list?
listSize1 :: [Text] -> Text
listSize1 []      = "Empty"
listSize1 [elem1] = "One element: " <> elem1
```

This is a dangerous function! Let's discover why:
- The first pattern `[]` will match an empty list.
- The second pattern `[elem1]` will match a single-element list.
- What happens the list has more than one element?

A runtime error happens! This is an appalling to a Haskell developer. The whole
point of using Haskell is to catch issues like this at compile time.

### A Safer Choice

Here is a safe example of list pattern matching:

```haskell
-- How big is a list?
-- Using the `_` pattern is a catch-all for any value
listSizeCatch :: [Text] -> Text
listSizeCatch []      = "Empty"
listSizeCatch [elem1] = "One element: " <> elem1
listSizeCatch _       = "More than one element"
```

The `_` pattern matches everything, so we know this function will work no matter
what the first value is.

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
their code.

### Matching With Constructors

We can use any data constructor in patterns, including the list constructor `:`:

```haskell
-- How big is a list?
listSizeCons :: [Text] -> Text
listSizeCons []         = "Empty"
listSizeCons (elem1:[]) = "One element: " <> elem1
listSizeCons (elem1:_)  = "More than one element, starting with: " <> elem1
```

Notice how the list constructor `:` is used in the patterns.
- In the `(elem1:[])` pattern, it constructs the value with an empty list `[]`.
  - This will only match a single-element list.
  - It is the same as writing `[elem1]`.
- In the `(elem1:_)` pattern, it constructs the value with the catch-all
  symbol `_`.
  - So this pattern will match any list with one or more element.
  - This pattern would match a single-element list, but it won't since it is
    listed *after* single-element pattern.

Order matters! The first matching pattern will be used, even if later patterns
would also match.

__Note__: Parentheses `()` are required to write patterns with constructors.

__Note__: We do not need the final catch-all symbol `_`, because we have covered
lists of every possible size: empty, one element, more than one element.

### Naming Entire Patterns

We have seen that we can use patterns to deconstruct values, but we can also
give names to the entire pattern.

We can use this feature to improve on the previous function and actually print
out the length of the list:

```haskell
-- How big is a list?
-- Note in the last pattern how the entire list is given the name `list1` using
-- the `@` symbol
-- Then this name is used to get the length of the entire list
listSizeName :: [Text] -> Text
listSizeName []              = "Empty"
listSizeName (elem1:[])      = "One element: " <> elem1
listSizeName list1@(elem1:_) = let
    listLenText 
        = T.pack       -- Convert String to Text
        $ show         -- Convert length to String
        $ length list1 -- Get the length of the list
    in
    listLenText <> " elements, starting with: " <> elem1
```

## Maybe

### Pattern Matching in Function Definition

Pattern matching provides a nice way to deconstruct `Maybe` values:

```haskell
maybeToText :: Maybe Text -> Text
maybeToText Nothing  = "No text! Nothing!"
maybeToText (Just t) = "The text is: " <> t
```

Notice how the value `t` can be used if the `(Just t)` pattern is matched. Of
course this value doesn't exist if the `Maybe` value is `Nothing`.

As with lists, the parentheses `()` are necessary when we are matching the
`Just` constructor.

### Pattern Matching in an Intermediate Value

Let's use pattern matching in a case statement to define an intermediate value:

```haskell
maybeWithCase :: Maybe Text -> Text
maybeWithCase mText = let
    textFound = case mText of
        Nothing -> "No text"
        (Just t) -> t
    in
    "I found: " <> textFound
```

### Pattern Matching with Guards

Pattern matching can be combined with guards for a more sophisticated logic:

```haskell
-- Check whether a Maybe Int is positive
-- Note how the guard is used on the same line as the pattern
maybePositive :: Maybe Int -> Maybe Int
maybePositive (Just x) | x >= 0 = Just x
maybePositive _ = Nothing
```

We do not need to use `otherwise` because the catch-all symbol (`_`) will 
catch any inputs that do not satisfy both the pattern and the guard.

## Either

Pattern matching is common for deconstructing Either values:

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

Notice how the catch-all symbol (`_`) is used in the first pattern, because we do
not need that argument.

## Tuples

Pattern matching is a common way to obtain the values held in tuples:

```haskell
-- Compute the area of a rectangle, given its height and width
rectangleArea :: (Double, Double) -> Double
rectangleArea (height, width) = height * width
```

## Records

Records include some additional pattern matching features to handle their 
named fields.

### Record Pattern Matching Syntax

```
functionName RecordDataConstructor1{}                      = (expression1)
functionName RecordDataConstructor2{ field1 = localName1 } = (expression2)
functionName RecordDataConstructor3
    { field1 = localName1, field2 = localName2, ... }      = (expression3)
```

### Example Record Type

Say we have this record type:

```haskell
data Vehicle
    = Aircraft
        { vaColor :: Text
        , vaWings :: Int
        }
    | Boat
        { vbColor :: Text 
        , vbSails :: Int
        }
    | Car 
        { vcColor  :: Text
        , vcWheels :: Int
        }
    deriving stock (Eq, Show)
```

### Pattern Matching the Record Constructor

```haskell
-- Can this vehicle fly in the air?
-- Even though we only want to match on the data constructor, we have to write
-- an `_` for each field in the record
vehicleCanFlyExplicit :: Vehicle -> Bool
vehicleCanFlyExplicit (Aircraft _ _) = True
vehicleCanFlyExplicit _              = False
```

A record type could potentially have a hundred fields, and we would not want to
write an `_` for each one. There is a better way:

```haskell
-- Same function as above, with a simpler syntax
-- Note how the curly braces indicate matching on the data constructor, but 
-- nothing after
vehicleCanFly :: Vehicle -> Bool
vehicleCanFly Aircraft{} = True
vehicleCanFly _          = False
```

The empty curly braces (`{}`) mean to fill in the rest of the pattern with
catch-all underscores (`_`).

### Pattern Matching with Record Fields

Curly braces can also be used to read fields of the record:

```haskell
-- What color is this vehicle?
-- Note how the contents of a field are given a name, which is then used 
-- in the function definition
vehicleColor :: Vehicle -> Text
vehicleColor Aircraft { vaColor = color } = "A " <> color <> " aircraft"
vehicleColor Boat     { vbColor = color } = "A " <> color <> " boat"
vehicleColor Car      { vcColor = color } = "A " <> color <> " car"
```

It is also possible to use more than one field:

```haskell
-- What kind of flying vehicle is this?
-- Note how more than one record field is used in the function definition
flyingVehicleType :: Vehicle -> Text
flyingVehicleType Aircraft 
    { vaColor = color, vaWings = wingQty }
    | wingQty == 0 = colorText <> "Zeppelin"
    | wingQty == 1 = colorText <> "Stealth Plane"
    | wingQty == 2 = colorText <> "Airplane"
    | wingQty  > 2 = colorText <> "Helicopter"
    | otherwise    = colorText <> "UFO"
  where
    colorText = "A " <> color <> " "
flyingVehicleType _ = "This vehicle can't fly" 
```

### Record Pattern Matching Conveniences

Two language extensions can add convenience to working with records and
patterns.

1. `RecordWildCards` can automaticlly define local names that exactly match the
   record field names.
2. `NamedFieldPuns` allows you to just input the record field name, and have it
   defined as a local name.

Feel free the explore these further once you feel confident writing record
patterns by hand.

