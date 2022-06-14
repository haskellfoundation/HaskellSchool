---
version: "1.0.0"
title: "Representing values as strings: Show and Read"
---

{% include toc.html %}

This lesson uses the deriving-strategies language extension.

```haskell
{-# language DerivingStrategies #-}
```

A few imports and other language extensions are introduced as needed.

## How GHCi prints results

Some things you enter into GHCi don't result in anything being printed in response. For example, imports:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> import Data.IORef
ghci> import Data.Foldable
```

And bindings, such as the following `IO` action which creates a new mutable reference and binds it to the a variable named `ref`.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> ref <- newIORef 1
```

If you enter an expression of type `IO ()`, then GHCi runs the action. For example, the following action multiplies the value of the mutable reference by the numbers 1, 2, 3, 4, and 5, thus computing 5 factorial. Still it prints nothing; the effect is performed silently and invisibly.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> traverse_ (\i -> modifyIORef' ref (* i)) [1 .. 5]
```

If you enter an expression whose type is `IO a` for some `a` other than `()`, then in addition to running the action, GHCi also then applies `print` to the result so you can see what it returns.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> readIORef ref :: IO Integer
120
```

GHCi runs the `IO` action that reads from the `IORef` to obtain the `Integer` value `120`. It then runs `print 120`, which prints the `String` value `"120"` to the terminal.

The effect you see when you type `readIORef ref` is the same as if you were to type `readIORef ref >>= print` instead.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> readIORef ref >>= print
120
```

If the expression you enter has some type other than `IO`, GHCi just applies `print` to show you the expression's evaluated value.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> product [1..5]
120
```

The effect when you type `product [1..5]` is, therefore, the same as if you were to type `print (product [1..5])` instead.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> print (product [1..5])
120
```

`product [1..5]` is the `Integer` value `120`, which is then converted to the String value `"120"` that we see.

There are some perfectly valid Haskell expressions that, if entered at the GHCi prompt, will elicit an error message.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> ref :: IORef Integer
error:
    • No instance for (Show (IORef Integer))
        arising from a use of ‘print’
```

Let's take a look at the type of the `print` function.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :type print
print :: Show a => a -> IO ()
```

Notice that it is polymorphic, which makes sense because GHCi can print many types of values. The class of types whose values can be shown in GHCi is called `Show`.

The definition of `print`, in the source code of the `base` package, looks like this:

```haskell
print :: Show a => a -> IO ()
print x = putStrLn (show x)
```

`show` is a method of the `Show` class that produces a `String` representation of the value.

### Exercise: print vs putStrLn

`print` and `putStrLn` serve similar roles, and their distinction can sometimes be subtle.

- What are the differences between their types?
- What are the differences in their behaviors?
- What is the difference between `print "hello"` and `putStrLn "hello"`?
- What is the type of `show @String`, and what does it do?

## What types belong to the Show class

Most basic datatypes have `Show` instances. Since a `Show` instance is involved in anything you see in GHCi, we have already seen many examples of showable types in previous lessons. If a type has an `Eq` instance, it can probably also have a `Show` instance.

Showability tends to extend upward into more complex types, because showing a complex value involves showing its constituent parts. For example, if `a` belongs to the `Show` class, then `Maybe a` and `[a]` do as well.

If we look at the list of instances for a type like `[]`, we see this fact expressed formally:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :info []
type [] :: * -> *
data [] a = [] | a : [a]
...
instance Show a => Show [a]
instance Read a => Read [a]
...
```

The line `instance Show a => Show [a]` means exactly what we said above. If the constraint `Show a` is satisfied, then the constraint `Show [a]` is satisfied. In other words, as long as we can print one item, then we can print a whole list of them.

### What types do not

Most notably, functions and `IO` actions do not belong to the `Show` class.

This is why, if you do not fully apply a function in GHCi, you see an error message.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> take 3

error:
    • No instance for (Show ([a0] -> [a0]))
        arising from a use of ‘print’
```

Complex types tend to be showable only if their constituent parts are showable. For example, just as `IO ()` does not belong to the `Show` class...

```console?lang=haskell&prompt=ghci>,ghci|
λ> show (putStrLn "hello")

error:
    • No instance for (Show (IO ())) arising from a use of ‘show’
```

Neither does `[IO ()]`.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> show [putStrLn "hello", putStrLn "world"]
error:
    • No instance for (Show (IO ())) arising from a use of ‘show’
```

We mentioned earlier that `Eq` and `Show` often go hand in hand. A rare counterexample can be found in the mutable references types [`IORef`](https://hackage.haskell.org/package/base/docs/Data-IORef.html), [`MVar`](https://hackage.haskell.org/package/base/docs/Control-Concurrent-MVar.html), and [`TVar`](https://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM-TVar.html#t:TVar), which belong to `Eq` but not `Show`. It is possible to test whether two things are the same `IORef`. But since accessing the values therein requires I/O, there is no `IORef Integer -> String` function that could display anything useful, therefore no `Show` instance.

### Unusual instances

Because the `Show` class is specifically for use by programmers in GHCi and not intended to be used for much else, there are no strict requirements for what constitutes an acceptable `Show` instance. Consequently, you will occasionally encounter a type that is not perfectly showable but been included in the `Show` class anyway.

For example, consider the `Handle` type. If we look at [the source code](https://hackage.haskell.org/package/base/docs/src/GHC-IO-Handle-Types.html#Handle) for this opaque type, we see that among its fields are some `String` values which can be shown and some `MVar`s which cannot. Despite the impossibility of displaying a full representation of a `Handle`, the authors have chosen to give the `Handle` type a `Show` instance that displays what information is available without requiring an I/O step to obtain it.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> import System.IO
ghci> stdout :: Handle
{handle: <stdout>}
ghci> openFile "/tmp/demo.txt" WriteMode :: IO Handle
{handle: /tmp/demo.txt}
```

In general, `show` should output a string that is valid Haskell code. As you can see in the demonstration above, the `Handle` instance also violates that notion.

## What Show is and isn't good for

The `Show` class, as we will discuss further below, is specifically designed to display Haskell expressions. It exists primarily to be used by Haskell development tools such as GHCi.

Other types of software that aren't Haskell development tools also often need to produce strings, but typically not in a Haskell-specific format, and so there often exists some means of constructing strings that is more appropriate than `Show` for a given purpose.

The `show` function is commonly used to produce `String` representations of numbers.

```haskell
{-# language OverloadedStrings, TypeApplications #-}

printDiskUsage :: Float -> IO ()
printDiskUsage x = putStrLn $
    "Disk usage is currently " <> show @Float (x * 100) <> "%"
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> printDiskUsage (2 / 3)
Disk usage is currently 66.66667%
ghci> printDiskUsage 0.0000000001
Disk usage is currently 1.0e-8%
```

We might instead prefer to use [`Text`](https://hackage.haskell.org/package/text) to build and print our messages. In addition to boasting improved efficiency over `String`, the `text` package offers a nice set functions for for displaying [integral](https://hackage.haskell.org/package/text/docs/Data-Text-Lazy-Builder-Int.html) and [floating-point](https://hackage.haskell.org/package/text/docs/Data-Text-Lazy-Builder-RealFloat.html) numbers. Below we give a revised example, this time specifying that we don't want to use scientific notation, and that we only care to see the fractional portion carried out to two decimal places.

```haskell
{-# language OverloadedStrings, TypeApplications #-}

import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Text.Lazy.Builder.RealFloat

printDiskUsage :: Float -> IO ()
printDiskUsage x = Text.putStrLn $ Text.toLazyText $
    "Disk usage is currently " <>
    formatRealFloat @Float
        Fixed      -- Don't use scientific notation
        (Just 2)   -- Limit to at most 2 decimal places
        (x * 100)
    <>  "%"
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> printDiskUsage (2 / 3)
Disk usage is currently 66.67%
ghci> printDiskUsage 0.0000000001
Disk usage is currently 0.00%
```

### Exercise: Print a Handle

We discussed `Handle` above. If you are going to write a log message that involves a `Handle`, you probably want to use [`hShow`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:hShow) instead. This function includes more information.

```haskell
hShow :: Handle -> IO String
```

Write a program that opens a file handle and uses `hShow` to print information about it to the terminal.

## Deriving Show

### Stock deriving

`Show` can be stock-derived for any enumeration type. We did this without mentioning it in the previous lesson.

```haskell
data Brightness = Light | Dark
    deriving stock Show

data Hue = Red | Green | Blue
    deriving stock Show
```

With the stock instance, `show` simply gives the name of the constructor.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> show Green
"Green"
```

To derive a stock `Show` instance for a type with constructor fields, all of the fields must belong to the `Show` class.

```haskell
data Color = Color Brightness Hue
    deriving stock Show
```

The result from `show` looks exactly like the Haskell expression that constructs a `Color`: the name of the constructor, followed by the result of applying `show` to each of the field values, separated by whitespace interspersed throughout.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> show (Color Light Blue)
"Color Light Blue"
```

If the datatype is defined as a record, the behavior of stock `Show` instance will reflect that.

```haskell
data ColorRecord = ColorRecord{ brightness :: Brightness, hue :: Hue }
    deriving stock Show
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> show (ColorRecord Light Blue)
"ColorRecord {brightness = Light, hue = Blue}"
```

If any of a type's constructor fields does not belong to the `Show` class, then `Show` cannot be stock-derived. In the following example, we attempt to derive `Show` for a record type that has an `IO ()` action as one of its fields.

```haskell
import Data.Text (Text)

data Message = Message { subject :: Text, body :: Text, send :: IO () }
    deriving stock Show
```

The third field of the `Message` constructor does not belong to the `Show` class, so compilation fails.

```
error:
    • No instance for (Show (IO ()))
        arising from the third field of ‘Message’ (type ‘IO ()’)
```

### Newtype deriving

If you define a datatype with a newtype-derived `Num` or `IsString` instance, you might want to also use newtype deriving for the `Show` instance.

#### With Num

Consider the `PageCount` example from the previous lesson.

```haskell
import Numeric.Natural (Natural)

newtype PageCount = PageCount Natural
    deriving stock Show
```

Giving this type a `Num` instance allows us to write `PageCount` values as numeric literals.

```haskell
{-# language GeneralizedNewtypeDeriving #-}

newtype PageCount = PageCount Natural
    deriving stock Show
    deriving newtype Num
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 5 :: PageCount
PageCount 5
```

If we also want `PageCount` values to be *shown* as numeric literals, then we can switch from the `stock` deriving strategy to the `newtype` strategy.

```haskell
newtype PageCount = PageCount Natural
    deriving newtype (Num, Show)
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 5 :: PageCount
5
```

`show (PageCount 5)` now evaluated to `"5"` rather than to `"PageCount 5"`.

#### With IsString

Likewise, it is common to derive the `IsString` class for newtypes that wrap a string-like type such as `Text`.

```haskell
{-# language GeneralizedNewtypeDeriving #-}

import Data.Text (Text)
import Data.String (IsString)

newtype CompanyName = CompanyName Text
    deriving stock Show
    deriving newtype IsString
```

With the `OverloadedStrings` extension, this allows company names to be written as string literals.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :set -XOverloadedStrings
ghci> "Acme" :: CompanyName
CompanyName "Acme"
```

However, company names still print with the `CompanyName` constructor. Our printed expressions can gain the same succinctness that `OverloadedStrings` provided to our code if we instead newtype-derive the `Show` instance.

```haskell
newtype CompanyName = CompanyName Text
    deriving newtype (IsString, Show)
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> "Acme" :: CompanyName
"Acme"
```

## Writing a Show instance by defining showsPrec

For opaque types, a derived `Show` instance would produce strings that involve constructors not available to the user. In this case, you should design a `Show` that produces an expression which a user of your module could actually copy into their own code. The `Seq`, `Map`, and `Set` types in the [`containers`](https://hackage.haskell.org/package/containers) package are common examples of opaque types. The `Percent` type from the previous lesson is another.

The complete `Show` class definition looks like this:

```haskell
class Show a
  where
    showsPrec :: Int -> a -> ShowS
    show :: a -> String
    showList :: [a] -> ShowS
    {-# MINIMAL showsPrec | show #-}

type ShowS = String -> String
```

The `show` method is the part of the `Show` class that we use most often, but the `showsPrec` definitions are doing most of the work behind the scenes. `showsPrec` is only used when writing a `Show` instance for a complex type. We apply `showsPrec` to each of the larger value's constituent parts, and it has some additional features that help incorporate the smaller strings into the larger result.

```haskell
showsPrec :: Show a => Int -> a -> ShowS
```

There are two interesting differences between `show` and `showsPrec`, both pertaining to how `Show` instances for complex types are built using the `Show` instances of their smaller parts:

- `showsPrec` has an additional `Int` parameter, which conveys some information about the context in which the displayed value is going to be incorporated into a larger expression.
- `showsPrec` has a return type of `ShowS` instead of `String`. This is a clever trick to allow efficient concatenation of the smaller string parts that come together into the final result.

### ShowS

`ShowS` is a type alias:

```haskell
type ShowS = String -> String
```

What the `S` stands for is a mystery, but the purpose of this type is not: This `String -> String` function represents a string as a [*difference list*](https://en.wikipedia.org/wiki/Difference_list). This is a way of representing a `String` as a function that prepends it to another `String`. Under this representation, we can use the function composition as concatenation, which is a constant-time operation that avoids the quadratic (slower) behavior exhibited by regular list concatenation. But we do not need to dwell on the how or why of difference lists at the moment. It suffices to know a few basic facts about working with `ShowS`:

In the `Prelude` module, there is a function [`showString`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:showString) for converting an ordinary `String` into its `ShowS` form.

```haskell
showString :: String -> ShowS
```

When strings are in `ShowS` form, use the function composition operator [`(.)`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:.) for concatenation.

To view the final result, you can convert back from `ShowS` to `String` by applying the `String -> String` function to `""`.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> (showString "one" . showString " " . showString "two") ""
"one two"
```

### The need for parentheses

Consider the following Haskell expressions, which are written below exactly as the relevant `Show` instances would display them:

- `Color Dark Green`
- `Just Green`
- `[Color Dark Green,Color Light Red]`
- `Just (Color Dark Green)`

The first three examples do not involve parentheses at all. The fourth has parentheses around the `Color` value. The parentheses are necessary because `Just Color Dark Green` would not mean the same thing.

When we show a value, how do we decide whether parentheses are needed?

First we have to understand how Haskell implicitly parenthesizes expressions when no parentheses are present. Consider the expression `1 + 2 * 3`. The standard arithmetic convention is to interpret this expression as `1 + (2 * 3)` rather than `(1 + 2) * 3`. The way we describe this rule is to say that `(*)` has a *higher precedence* than `(+)`, or sometimes we say that multiplication *associates more tightly* than addition; the 2 sort of "sticks to" the 3, rather than to the 1, because the `(*)` is in some sense *stronger* than `(+)`.

### Precedence

In Haskell, every infix operator is assigned a precedence between 0 and 9. A handful of examples:

|------------|-----------------------------------------------------------------------------------|---------------------------|
| Precedence | Operator                                                                          | Significance              |
|------------|-----------------------------------------------------------------------------------|---------------------------|
| 0          | [`($)`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:-36-)        | Function application      |
| 2          | [`(||)`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:-124--124-) | Logical disjunction       |
| 3          | [`(&&)`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:-38--38-)   | Logical conjunction       |
| 6          | [`(+)`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:-43-)        | Arithmetic addition       |
| 7          | [`(*)`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:-42-)        | Arithmetic multiplication |
| 9          | [`(.)`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:.)           | Function composition      |
| 10         | whitespace                                                                        | Function application      |
|------------|-----------------------------------------------------------------------------------|---------------------------|

The space between identifiers can be thought of as sort of invisible operator having a precedence of 10, higher than all infix operators. Consider, then, the expression `f 1 + g 2 * h 3`. This is read as `(f 1) + ((g 2) * (h 3))`.

It might not be obvious what any of this has to do with `Show`, because data constructors are rarely infix operators. But sometimes they are. A constructor name is allowed to be an operator, with the restriction that its first character must be a colon (`:`).

```haskell
infixr 6 :+
infixl 7 :*
data Expr =
    Number Integer
  | Expr :+ Expr    -- ^ Addition
  | Expr :* Expr    -- ^ Multiplication
    deriving stock Show

evaluate :: Expr -> Integer
evaluate expr =
    case expr of
        Number x  ->  x
        a :+ b    ->  evaluate a + evaluate b
        a :* b    ->  evaluate a * evaluate b
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> evaluate (Number 1 :+ Number 2 :* Number 3)
7
```

Here we have given a small datatype representing arithmetic expressions involving addition and multiplication. We have given our constructors `(:+)` and `(:*)` the same precedences as `(+)` and `(*)` -- 6 and 7, respectively -- so that the multiplication constructor binds more tightly than the addition constructor. Below we demonstrate the behavior of the stock `Show` instance.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> Number 1 :+ Number 2 :* Number 3
Number 1 :+ Number 2 :* Number 3
ghci> (Number 1 :+ Number 2) :* Number 3
(Number 1 :+ Number 2) :* Number 3
```

For the first value, the automatically derived `Show` code did not inserted any parentheses. For the second, it recognized that parentheses were needed to preserve the association of 1 and 2.

### The Int controls the parentheses

The `Int` parameter of `showsPrec` specifies the precedence of the surrounding context to determine whether parentheses are needed when showing a value. It is given as the surrounding precedence **plus one**, for reasons that are lost to time. If the surrounding precedence *at least* the precedence of the constructor being shown (equivalently, if the surrounding precedence-plus-one is *greater than* the precedence of the constructor being shown), then parentheses are required in order to maintain the association of the contents within the shown sub-expression.

For example, consider the expression `Just 1`. The `Just` constructor is applied using ordinary function application syntax, which has precedence 10. If the surrounding context has precedence 9 or less, no parentheses are needed:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> showsPrec (9 + 1) (Just 1) ""
"Just 1"
```

But if the surrounding context is 10, then we get parentheses in the output of `showsPrec`:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> showsPrec (10 + 1) (Just 1) ""
"(Just 1)"
```

The `Prelude` module provides a [`showParen`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:showParen) function for this purpose; it conditionally surrounds a string in parentheses.

```haskell
showParen :: Bool -> ShowS -> ShowS
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> showParen True (showString "abc") ""
"(abc)"
ghci> showParen False (showString "abc") ""
"abc"
```

Since constructors are not usually infix operators, most often the surrounding precedence is either `10` (when showing a value that appears as a field in an ordinary constructor) or `-1` (when showing a value with no surrounding context, in which case we never need parentheses).

### Minimal definition

When we write a custom `Show` instance, we only need to define `showsPrec`.

The default implementation of the `show` method is:

```haskell
show x = showsPrec (precedence + 1) x ""
  where
    precedence = -1
```

It applies `showsPrec` to the value with an `Int` parameter of `0` to indicate that the surrounding precedence is `-1`. The expression `showsPrec 0 x` has the type `ShowS`. This is then applied to `""` to convert the result to `String`.

### Example

Let's walk through a small example where we write a `Show` instance that behaves exactly the same as the stock-derived instance would.

```haskell
data Name = Anonymous | Name String
```

```haskell
instance Show Name
  where
    showsPrec surroundingPrecedencePlusOne name =
        case name of
            Anonymous -> showString "Anonymous"
            Name x    -> showParen (surroundingPrecedencePlusOne > precedence) $
                             showString "Name " . showsPrec (precedence + 1) x
              where
                precedence = 10
```

- `Anonymous` is never surrounded by parentheses because it is an "atomic" expression: it doesn't contain any smaller pieces that need help to stay grouped together.
- The `Name` constructor is applied to one argument using ordinary function application, which has precedence 10. If the surrounding precedence is at least 10, then the `Name ...` expression will be parenthesized.

To test our `Show` instance, it is not enough to simply print some `Name` values.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> Anonymous
Anonymous
ghci> Name "The Motel"
Name "The Motel"
```

We should also test some larger structures that contain `Name` fields, to ensure that the `Name` portions are enclosed in parentheses as necessary.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> Just Anonymous
Just Anonymous
ghci> Just (Name "The Motel")
Just (Name "The Motel")
```

### Exercise: Expr

Remove the `deriving` clause from the `Expr` type above, and instead write a `Show` instance that behaves the same as the derived instance. Use the `Show` instance that we wrote for `Name` as a guide.

### Exercise: Percent

Consider the `Percent` module from the previous lesson.

```haskell
module Percent (Percent, toInt, clamp) where

newtype Percent = Percent Int
    deriving stock Show

toInt :: Percent -> Int
toInt (Percent x) = x

clamp :: Int -> Percent
clamp x
    | x <= 0    = Percent 0
    | x >= 100  = Percent 100
    | otherwise = Percent x
```

If a user of this module prints a `Percent` value, the `Show` instance will give them an expression involving the `Percent` constructor.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> import qualified Percent
ghci> Percent.clamp 75
Percent 75
```

Since the `Percent` constructor is not exported by the module, this is not an ideal string for the user to see. It is not an expression that they would be able to write themselves.

Remove the `deriving` clause from `Percent`, and replace it with a custom `Show` instance that behaves as follows:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> Percent.clamp 75
Percent.clamp 75
ghci> Right (Percent.clamp 110)
Right (Percent.clamp 100)
```

## Showing functions

We noted earlier that the function type `(->)` has no `Show` instance. Could there be? Not in general, but what if the function's domain is small? We could produce a list that shows what the function returns for every possible input, and this would show us everything that there is to know about the function.

```haskell
newtype BoolFunction a = BoolFunction (Bool -> a)

instance Show a => Show (BoolFunction a)
  where
    showsPrec i (BoolFunction f) =
        showsPrec i [(False, f False), (True, f True)]

fiftyIfTrue :: Bool -> Integer
fiftyIfTrue x = case x of
    True -> 50
    False -> 0
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> BoolFunction fiftyIfTrue
[(False,0),(True,50)]
```

### Exercise: Show with Enum and Bounded

Complete the instance definition below with a generalization of what we did for `BoolFunction` above.

```haskell
newtype FiniteFunction a b = FiniteFunction (a -> b)

instance (Enum a, Bounded a, Show a, Show b) => Show (FiniteFunction a b)
```

## Read: The opposite of Show

The `Read` class provides a system similar to `showsPrec` for parsing Haskell expressions. The chief way to use `Read` is via the [`readMaybe`](https://hackage.haskell.org/package/base/docs/Text-Read.html#v:readMaybe) function in the `Text.Read` module.

```haskell
readMaybe :: Read a => String -> Maybe a
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :set -XTypeApplications
ghci> readMaybe @[Integer] "[1,2,3,4]"
Just [1,2,3,4]
ghci> readMaybe @[Integer] "[1,2"
Nothing
```

It is generally the case that `readMaybe . show = Just` -- in other words, the `Read` instance should be able to read anything that the `Show` instance produces.

`Read` can be stock-derived under the same conditions as `Show`: for any enumeration type, and for any type whose constructor fields all have `Read` instances.

Whereas the `Show` class is extremely common, `Read` enjoys far less use. For this reason, we will not delve into it further here.
