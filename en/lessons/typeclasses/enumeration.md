---
version: "1.0.0"
title: "Listing a type's values: Enum and Bounded"
---

{% include toc.html %}

This lesson uses only the `base` package. It requires imports of a few standard types:

```haskell
import Data.Word (Word8)
import Numeric.Natural (Natural)
```

We also use the deriving-strategies language extension:

```haskell
{-# language DerivingStrategies #-}
```

A few other imports and language extensions will be introduced where they are used below.

## Enum: Enumerable types

The `Enum` class is for types whose values can be produced in sequential order. These are typically very simple types, representing concepts like:

- 1, 2, 3, 4, 5, ...
- a, b, c, d, e, ...
- false, true
- clubs, diamonds, hearts, spades

### Numeric ranges

The most common use for `Enum` is to generate an ascending sequence of integers using its `enumFromTo` method.

```haskell
enumFromTo :: Enum a => a -> a -> [a]
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> enumFromTo 8 12 :: [Integer]
[8,9,10,11,12]
```

The `Enum` class also has an `enumFrom` method which has only the start parameter.

```haskell
enumFrom :: Enum a => a -> [a]
```

In the case of `Integer`, which is unbounded, this generates an infinite list.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> enumFrom 8 :: [Integer]
[8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23, ... (goes on indefinitely)
ghci> take 3 (enumFrom 8) :: [Integer]
[8,9,10]
```

Not all numeric types go on indefinitely; some have an upper limit. For example, `Word8` stops at 255.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> enumFrom 240 :: [Word8]
[240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255]
```

There is a special built-in way to write `enumFromTo` and `enumFrom` expressions, called "range syntax".

* `[x ..] = enumFrom x`
* `[x .. y] = enumFromTo x y`

```console?lang=haskell&prompt=ghci>,ghci|
ghci> [8 .. 12] :: [Integer]
[8,9,10,11,12]
ghci> [240 ..] :: [Word8]
[240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255]
```

#### Increments other than (+ 1)

With both `enumFrom` and `enumFromTo`, each item in the sequence is always 1 greater than the previous. Going backward is not allowed, and attempting to do so will only produce an empty result.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> [10 .. 5] :: [Integer]
[]
```

There two less commonly used methods that also have their own special forms. Whereas `enumFrom` and `enumFromTo` are only for incrementing upwards by one, their "then" variants can elicit enumeration in larger increments or in reverse order.

* `[x, y ..] = enumFromThen x y`
* `[x, y .. z] = enumFromThenTo x y z`

```console?lang=haskell&prompt=ghci>,ghci|
ghci> [240, 245 ..] :: [Word8]
[240,245,250,255]
ghci> [1, 3 .. 10] :: [Integer]
[1,3,5,7,9]
ghci> [10, 9 .. 5] :: [Integer]
[10,9,8,7,6,5]
```

For types that have a lower bound, such as the `Natural` type which does not include negative numbers, the descent stops at the lower limit.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> [10, 9 ..] :: [Natural]
[10,9,8,7,6,5,4,3,2,1,0]
```

#### Summary of range syntax

|---------------|------------------------|----------------------------------------------------------------------|
| Special form  | Written normally       | Example                                                              |
|---------------|------------------------|----------------------------------------------------------------------|
| `[x ..]`      | `enumFrom x`           | `[x ..] = [1, 2, 3, 4, 5, 6, 7, 8, 9, {- continues indefinitely -}]` |
| `[x .. y]`    | `enumFromTo x y`       | `[1 .. 5] = [1, 2, 3, 4, 5]`                                         |
| `[x, y ..]`   | `enumFromThen x y`     | `[1, 3 ..] = [1, 3, 5, 7, 9, {- continues indefinitely -}]`          |
| `[x, y .. z]` | `enumFromThenTo x y z` | `[1, 3 .. 10] = [1, 3, 5, 7, 9]`                                     |
|---------------|------------------------|----------------------------------------------------------------------|

### Deriving Enum

#### Stock deriving

A stock instance of `Enum` can be derived for any *enumeration type*. An enumeration type is one whose constructors have no fields. `Bool` and `Ordering` are two examples of enumeration types, and they both belong to the `Enum` class. Their definitions look like this:

```haskell
data Bool = False | True

data Ordering = LT | EQ | GT
```

Let's define a datatype representing denominations of paper currency.

```haskell
data Bill = One | Five | Ten | Twenty | Fifty | Hundred
    deriving stock Show
```

Suppose we want a list of all bills above 10. Presently, this fails because `Bill` does not belong to the `Enum` class:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> enumFrom Ten

error:
    • No instance for (Enum Bill)
```

The `Bill` type has six constructors. None of the constructors has any fields, therefore `Bill` is an enumeration type, and we can get a derived `Enum` instance for it. Modify the definition of `Bill` by adding `Enum` to the list of derived classes:

```haskell
data Bill = One | Five | Ten | Twenty | Fifty | Hundred
    deriving stock (Enum, Show)
```

Now we get the desired result.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> enumFrom Ten
[Ten,Twenty,Fifty,Hundred]
```

The constructors are enumerated in the same order in which they appear in the type definition.

#### Newtype deriving

Another way to get an `Enum` instance is to use newtype deriving. It is common to define a `newtype` that simply wraps another, to imbue it with some addition domain-specific connotation. For example, suppose we have a type that represents the number of pages in a printable document.

```haskell
newtype PageCount = PageCount Natural
    deriving stock Show
```

`PageCount` is not an enumeration type, so `Enum` cannot be stock derived. However, it is a `newtype` for `Natural`, and `Natural` belongs to the `Enum` class. Therefore `Enum` can be `newtype` derived.

```haskell
{-# language GeneralizedNewtypeDeriving #-}

newtype PageCount = PageCount Natural
    deriving stock Show
    deriving newtype Enum
```

`PageCount` can now be enumerated, and the enumeration behaves the same as `Natural`.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> [PageCount 1 .. PageCount 5]
[PageCount 1,PageCount 2,PageCount 3,PageCount 4,PageCount 5]
```

### Conversion with Int

The `Enum` class also has a pair of methods that represent a correspondence between the enumerable type and `Int`.

```haskell
toEnum   :: Enum a => Int -> a
fromEnum :: Enum a => a -> Int
```

Returning to the earlier `Bill` example, the `Int` values that the compiler has automatically associated with each constructor are:

```haskell
data Bill = One | Five | Ten | Twenty | Fifty | Hundred   deriving stock (Enum, Show)
         --  0     1      2      3        4        5
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> fromEnum Twenty
3
ghci> toEnum 4 :: Bill
Fifty
```

For most types, the `toEnum` function is partial. An enumeration like `Bill` has only a fixed range (in this case, 0 to 5), and so `toEnum` fails when given an `Int` that it outside of this range.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> toEnum 7 :: Bill
*** Exception: toEnum{Bill}: tag (7) is outside of enumeration's range (0,5)
```

Notice that the `Int` values have no relationship whatsoever with the *meanings* of the constructors; `Twenty` has been assigned the number 3, not 20. The `Int`s are not intended to mean anything; `toEnum`/`fromEnum` is merely a mechanism that is used to efficiently generate ranges. For example, the default definition of the `enumFromTo` is:

```haskell
enumFromTo x y =
    map toEnum (
        [fromEnum x .. fromEnum y] :: [Int]
    )
```

In other words, every range of `Bill`s (or any other `Enum` type) actually begins its life as an `Int` range, which is then converted to the appropriate type using `toEnum`.

If you do need a function that relates each `Bill` constructor to its numeric meaning, you can write that function separately.

```haskell
billAmount :: Bill -> Natural
billAmount x = case x of
    One     -> 1
    Five    -> 5
    Ten     -> 10
    Twenty  -> 20
    Fifty   -> 50
    Hundred -> 100
```

But this has nothing to do with the `Enum` class.

### Writing Enum instances

One rarely write a custom `Enum` instance but in few circumstances:

- For a constructor with multiple fields where `Enum` cannot be derived
- For a refinement `newtype` that imposes restricted bounds on its underlying enumerable type

#### With multiple fields

Consider for example a type representing a position on tic-tac-toe board.

```haskell
data Position =
    NorthWest | North  | NorthEast
  | West      | Center | East
  | SouthWest | South  | SouthEast
  deriving stock (Enum, Show)
```

This is all well and good, and we have an `Enum` instance. But suppose we then decide to define a position as a record comprised of horizontal and vertical positions.

```haskell
data Horizontal = West | CenterX | East
    deriving stock (Enum, Show)

data Vertical = North | CenterY | South
    deriving stock (Enum, Show)

data PositionXY = PositionXY
    { positionX :: Horizontal
    , positionY :: Vertical }
    deriving stock (Enum, Show)
```

We get a compilation failure:

```
error:
    • Can't make a derived instance of ‘Enum PositionXY’:
        ‘PositionXY’ must be an enumeration type
```

The `Position` and `PositionXY` types are not much different from one another -- they still represent the same nine positions -- but since `PositionXY` is neither an enumeration type nor a `newtype`, the automatic deriving mechanisms discussed so far cannot help us. Remove `Enum` from the `deriving` clause in the `PositionXY` type definition.

Use the GHCi `:info` command to inspect the `Enum` class.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :info Enum
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
```

There are a lot of methods there, but we don't need to define them all. The `MINIMAL` pragma tells us that an `Enum` instance must include definitions for `toEnum` and `fromEnum`. All the rest of the methods have default definitions.

The simplest way to write an `Enum` instance for `PositionXY` is to list all nine mappings in the range 0 to 8.

```haskell
import GHC.Enum (toEnumError)

instance Enum PositionXY
  where
    fromEnum p = case p of
        PositionXY West    North    -> 0
        PositionXY CenterX North    -> 1
        PositionXY East    North    -> 2
        PositionXY West    CenterY  -> 3
        PositionXY CenterX CenterY  -> 4
        PositionXY East    CenterY  -> 5
        PositionXY West    South    -> 6
        PositionXY CenterX South    -> 7
        PositionXY East    South    -> 8
    toEnum i = case i of
        0 -> PositionXY West    North
        1 -> PositionXY CenterX North
        2 -> PositionXY East    North
        3 -> PositionXY West    CenterY
        4 -> PositionXY CenterX CenterY
        5 -> PositionXY East    CenterY
        6 -> PositionXY West    South
        7 -> PositionXY CenterX South
        8 -> PositionXY East    South
        _ -> toEnumError "PositionXY" i (0, 8)
```

One could introduce some arithmetic tricks to shorten this code, but we'll leave it at that. Later in this lesson, we will see an easier way to automatically get `Enum` instances for types like these.

#### With restricted bounds

Sometimes we use module encapsulation to impose restrictions on what values are allowed in particular fields. To demonstrate, below we define a datatype called `Percent`, and we constrain it to have only integer values between 0 and 100. Because the enclosing module does not export the `Percent` constructor, it is not possible for a user of the module to violate this restriction by constructing a value of, say, `Percent 110`.

```haskell
module Percent (Percent, toInt, clamp, validate) where

newtype Percent = Percent Int
    deriving stock Show

toInt :: Percent -> Int
toInt (Percent x) = x

clamp :: Int -> Percent
clamp x
    | x <= 0    = Percent 0
    | x >= 100  = Percent 100
    | otherwise = Percent x

validate :: Int -> Maybe Percent
validate x
    | x <= 0    = Nothing
    | x >= 100  = Nothing
    | otherwise = Just (Percent x)
```

A newtype-derived `Enum` instance for the `Percent` type would not do what we want, because then `toEnum 110` would allow a user to construct an erroneous `Percent` value. We need a custom `Enum` instance that imposes the desired bounds. In addition to `fromEnum` and `toEnum`, most of the method defaults for `Enum` assume an unbounded type and thus need to be overridden by our instance to impose the upper and lower bounds.

- `succ` and `pred` produce the next and previous value, respectively, in the sequence. They produce an `error` when there is no next or previous value. In our case, `pred 0` and `succ 100` should fail.
- `enumFrom`, the method that iterates upward incrementing by 1, needs to be defined such that the range stops at 100.
- `enumFromThen`, the generalization of `enumFrom` that can generate a range that goes either upward or downward, needs to stop at 100 when going upward and stop at 0 when going downward.

```haskell
import GHC.Enum (predError, succError, toEnumError)

instance Enum Percent
  where
    fromEnum = toInt
    toEnum x = case validate x of
        Just p -> p
        Nothing -> toEnumError "Percent" x (0, 100)
    succ (Percent x)
        | x == 100 = succError "Percent"
        | otherwise = Percent (x + 1)
    pred (Percent x)
        | x == 0 = predError "Percent"
        | otherwise = Percent (x - 1)
    enumFrom (Percent x) =
        map Percent (enumFromTo x 100)
    enumFromThen (Percent x) (Percent y)
        | x < y     = map Percent (enumFromThenTo x y 100)
        | otherwise = map Percent (enumFromThenTo x y 0)
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> [Percent.clamp 5, Percent.clamp 4 ..]
[Percent 5,Percent 4,Percent 3,Percent 2,Percent 1,Percent 0]
ghci> [Percent.clamp 95, Percent.clamp 97 ..]
[Percent 95,Percent 97,Percent 99]
```

### Relationship with Ord

`Enum` and `Ord` represent related ideas, and it is often the case that if a type belongs to the `Enum` class, then it also belongs to the `Ord` class. Typically, `a < b` if `a` appears before `b` in the enumeration order.

Though both classes represent an ordering, the two classes represent that ordering in different ways. `Ord` only provides methods for pairwise testing -- how does `a` compare to `b`? -- whereas `Enum` provides not only a means of pairwise testing (via the correspondence with `Int`), but also a way to enumerate the elements in order. The latter task cannot be accomplished using only the methods of `Ord`.

In the tic-tac-toe example above, if we want to make sure that our `Ord` instance is compatible with our custom `Enum` instance, then we may define it as follows:

```haskell
instance Eq PositionXY where
  where
    (==) = (==) `on` fromEnum

instance Ord PositionXY
  where
    compare = compare `on` fromEnum
```

In other words, these instance definitions state that the way to compare two `PositionXY`s is to compare the corresponding `Int` ordinals assigned to them by the `Enum` instance.

### Peculiar instances

`Float`, `Double`, and `Rational` have exceptional `Enum` instances that disagree with some of the things we have said about how an `Enum` generally behaves. These instances only behave sensibly when dealing with whole numbers, and in general the use of `Enum` methods with these fractional types should be avoided.

## Bounded: Upper and lower limits

`Bounded` is a small class consisting of two methods:

```haskell
class Bounded a where
    minBound :: a
    maxBound :: a
```

### Types in the Bounded class

There are generally two varieties of type that belong to the `Bounded` class:

  - Enumeration types
  - Numeric types that have a fixed size

Bounded numbers include:

  - Everything in the [`Data.Int`](https://hackage.haskell.org/package/base/docs/Data-Int.html) module: `Int`, `Int8`, `Int16`, `Int32`, `Int64`
  - Everything in the [`Data.Word`](https://hackage.haskell.org/package/base/docs/Data-Word.html) module: `Word`, `Word8`, `Word16`, `Word32`, `Word64`

These types represent subsets of [`Integer`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Integer) and [`Natural`](https://hackage.haskell.org/package/base/docs/Numeric-Natural.html), respectively. `Integer` and `Natural` values do not have any limit imposed upon on their size, so although they belong to `Enum`, they do not belong to `Bounded`.

### Relationship with Ord

Since "minimum" and "maximum" are concepts related to ordering, it is often the case that if a type belongs to the `Bounded` class, then it also belongs to the `Ord` class. Typically it should be the case for any value `x` that `x >= minBound` and `x <= maxBound`.

### Complete enumeration

When a type belongs to both `Enum` and `Bounded`, the `minBound` should be the first item in the enumeration sequence, and `maxBound` should be the last. Therefore the expression `[minBound ..]` is equivalent to `[minBound .. maxBound]`) and gives a list of all values of that type.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> [minBound ..] :: [Bool]
[False,True]
ghci> [minBound ..] :: [Ordering]
ghci> [minBound ..] :: [Word8]
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255]
```

This is the chief function of the `Bounded` class.

### Exercise: Manual Bounded instances

* Write a `Bounded` instance for `Percent`, and use `[minBound ..]` in GHCi to generate a list of all the percents.

* Write a `Bounded` instance for `PositionXY`, and use `[minBound ..]` in GHCi to generate a list of all the positions.

### Deriving Bounded

Like `Enum`, the `Bounded` class can be stock-derived for any enumeration type.

Suppose we want to define a datatype that represents the four [XMPP](https://xmpp.org/) availability states.

```haskell
data ChatStatus = Away | Available | DoNotDisturb | ExtendedAway
    deriving stock (Eq, Ord, Show)
```

Because this is an enumeration type (it has no fields), we can derive stock `Enum` and `Bounded` instances for it by simply adding these classes to the `deriving` clause.

```haskell
data ChatStatus = Away | Available | DoNotDisturb | ExtendedAway
    deriving stock (Eq, Ord, Show, Enum, Bounded)
```

The first constructor is the "min bound", the last constructor is the "max bound", and `[minBound ..]` lists all of the constructors in order.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> minBound :: ChatStatus
Away
ghci> maxBound :: ChatStatus
ExtendedAway
ghci> [minBound ..] :: [ChatStatus]
[Away,Available,DoNotDisturb,ExtendedAway]
```

### Deriving for more complicated datatypes

Suppose we have the following simple definition of a color:

```haskell
data Brightness = Light | Dark

data Hue = Red | Green | Blue

data Color = Color Brightness Hue
```

`Brightness` and `Hue` are enumeration types, so we can derive stock instance of `Enum` and `Bounded` for both of them. `Color`, however, is not an enumeration type, because its constructor has fields. Stock deriving of `Enum` and `Bounded` is not supported for this `Color` type.

It is still possible to automatically derive `Enum` and `Bounded` for `Color`, but we need more advanced means: the [`generic-data`](https://hackage.haskell.org/package/generic-data) library and a few language extensions that introduce additional deriving mechanisms.

```haskell
{-# language DeriveGeneric, DerivingVia #-}

import Generic.Data (Generic, FiniteEnumeration (..))

data Brightness = Light | Dark
    deriving stock (Enum, Bounded, Show)

data Hue = Red | Green | Blue
    deriving stock (Enum, Bounded, Show)

data Color = Color Brightness Hue
    deriving stock (Generic, Show)
    deriving (Enum, Bounded) via (FiniteEnumeration Color)
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> enumFromTo minBound maxBound :: [Color]
[Color Light Red,Color Light Green,Color Light Blue,Color Dark Red,Color Dark Green,Color Dark Blue]
```

The `Enum` and `Bounded` instances here are obtained via the [`FiniteEnumeration`](https://hackage.haskell.org/package/generic-data/docs/Generic-Data.html#t:FiniteEnumeration) newtype. This instance maps each (`Brightness`, `Hue`) pair to an `Int` to form an enumeration sequence in much the same way as the instance we wrote for `PositionXY` earlier, but in an automated way that makes use of [generics](https://hackage.haskell.org/package/base/docs/GHC-Generics.html). This particular approach to deriving requires that all of the constructor's field types (here, `Brightness` and `Hue`) have instances of both `Enum` and `Bounded`.

#### Exercise: Enum PositionXY

Let's revisit the `PositionXY` type we discussed earlier.

```haskell
data PositionXY = PositionXY
    { positionX :: Horizontal
    , positionY :: Vertical }
```

We gave this type an `Enum` instance by listing all nine cases in the definitions of `fromEnum` and `toEnum`.

```haskell
instance Enum PositionXY
  where
    fromEnum p = case p of
        PositionXY West    North    -> 0
        PositionXY CenterX North    -> 1
        ...
    toEnum i = case i of
        0 -> PositionXY West    North
        1 -> PositionXY CenterX North
        ...
```

Remove this `Enum` instance, and in its place derive an `Enum` instance via `FiniteEnumeration PositionXY`, following the example of `Color`.
