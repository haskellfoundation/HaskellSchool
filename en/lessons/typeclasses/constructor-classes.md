---
version: "1.0.0"
title: "Constructor class basics: Foldable and Functor"
---

{% include toc.html %}

This lesson covers topics from these modules:

```haskell
import Data.Foldable
import Data.Functor
```

We will discuss the following functions:

|----------------------------------------------------------------------------------------------|------------------------------------------------------------------|
| Function                                                                                     | Type                                                             |
|----------------------------------------------------------------------------------------------|------------------------------------------------------------------|
| [`toList`](https://hackage.haskell.org/package/base/docs/Data-Foldable.html#v:toList)        | `(Foldable t) => t a -> [a]`                                     |
| [`foldr`](https://hackage.haskell.org/package/base/docs/Data-Foldable.html#v:foldr)          | `(Foldable t) => (a -> b -> b) -> b -> t a -> b`                 |
| [`foldMap`](https://hackage.haskell.org/package/base/docs/Data-Foldable.html#v:foldMap)      | `(Foldable t, Monoid b) => (a -> b) -> t a -> b`                 |
| [`fmap`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:fmap)                  | `(Functor f) => (a -> b) -> f a -> f b`                          |
|----------------------------------------------------------------------------------------------|------------------------------------------------------------------|

The subject of this lesson is closely related to "container" datatypes, and we will be looking at several interesting examples from the `containers` library:

```haskell
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Tree (Tree (..))
import qualified Data.Tree as Tree
```

Some examples involve constructing text values. As demonstrated in the lesson on `Show`, for this we use the `text` library and the `OverloadedStrings` language extension.

```haskell
{-# language OverloadedStrings #-}

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder
```

The examples also use a few other miscellaneous imports from the `base` library;

<!-- If they ever add an 'mtimes' method to Monoid, please change this to use that instead of mtimesDefault. -->

```haskell
import Data.Function ((&))
import Data.Functor.Const (Const (..))
import Data.Semigroup (mtimesDefault)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import System.Environment (getEnvironment)
```

The `Foldable` and `Functor` types can all be stock-derived. As usual, we will explicitly use the `stock` deriving strategy.

```haskell
{-# language DerivingStrategies #-}
```

Unlike the classes previously discussed, each of the classes here requires a language extension in order to be stock-derived.

```haskell
{-# language DeriveFoldable, DeriveFunctor #-}
```

## Containers and constructor classes

What do all of the following datatypes have in common?

|-----------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------|
| Datatype                                                                    | Notes                                                                                                                       |
|-----------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------|
| `data Maybe item = Nothing | Just item`                                     | From the [base](https://hackage.haskell.org/package/base/docs/Data-Maybe.html) package                                      |
| `data Name item = Anonymous | Name item`                                    |                                                                                                                             |
| `data List item = Nil | Cons item (List item)`                              | Equivalent to the built-in `[]` type                                                                                        |
| `data Numbered item = Numbered{ ordinal :: Natural, numberedItem :: item }` |                                                                                                                             |
| `data Tuple a item = Tuple a item`                                          | Equivalent to the built-in `(,)` type                                                                                       |
| `data Pair item = Pair item item`                                           |                                                                                                                             |
| `data Const a item = Const a`                                               | Equivalent to `Const` in the [base](https://hackage.haskell.org/package/base/docs/Data-Functor-Const.html) library) |
| `data Set item`                                                             | Opaque type in the [containers](https://hackage.haskell.org/package/containers/docs/Data-Set.html) library                  |
| `data Map key item`                                                         | Opaque type in the [containers](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html) library           |
| `data Tree item = Node{ rootLabel :: item, subForest :: [Tree item] }`      | From the [containers](https://hackage.haskell.org/package/containers/docs/Data-Tree.html) library                           |
|-----------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------|

Not much, but one important similarity: They all have at least one type parameter. In the list above, we have written each type with `item` as the name of the final type variable. If we write this list with all the `item` type variables omitted, it reads as:

|------------------|-----------------------------------------------------------------------------|
| Type constructor | Datatype                                                                    |
|------------------|-----------------------------------------------------------------------------|
| `Maybe`          | `data Maybe item = Nothing | Just item`                                     |
| `Name`           | `data Name item = Anonymous | Name item`                                    |
| `List`           | `data List item = Nil | Cons item (List item)`                              |
| `Numbered`       | `data Numbered item = Numbered{ ordinal :: Natural, numberedItem :: item }` |
| `Tuple a`        | `data Tuple b item = Tuple b item`                                          |
| `Pair`           | `data Pair item = Pair item item`                                           |
| `Const a`        | `data Const b item = Const b`                                               |
| `Set`            | `data Set item`                                                             |
| `Map key`        | `data Map key item`                                                         |
| `Tree`           | `data Tree item = Node{ rootLabel :: item, subForest :: [Tree item] }`      |
|------------------|-----------------------------------------------------------------------------|

These are called *type constructors*; they're something that is like a type, but not quite. A type constructor has to be applied to one more type argument to become an actual type. There is no such thing as a value of type `Maybe`, but there are values of `Maybe Integer`. Likewise, you can't have a `Map Text`, but you can have a `Map Text Text`. But just because `Map Text` is not itself a type that has values doesn't mean it isn't something interesting that we can talk about -- it just means it's not a type, but rather a type constructor. It's a different kind of thing, so we'll be discussing different kinds of classes that such a thing might belong to. Among these classes are the subjects of this lesson: `Foldable` and `Functor`. They are classes to which type constructors, not types, can belong. Accordingly, sometimes they are referred to as *constructor classes* rather than typeclasses.

What else do all of these type constructors have in common? Here's one thing: Take any of these type constructors and call it `container`. Given a value of type `container item`, we can extract a list of all the `item` values it contains. A few examples:

|----------------------------------------------------------|---------------------------|-------------------------------------------------------------|
| Container                                                | Contains these `a` values | Notes                                                       |
|----------------------------------------------------------|---------------------------|-------------------------------------------------------------|
| `Nothing :: Maybe Text`                                  | `[ ]`                     |                                                             |
| `Just "Hello" :: Maybe Text`                             | `[ "Hello" ]`             |                                                             |
| `Numbered 3 "Bronze" :: Numbered Text`                   | `[ "Bronze" ]`            | The `3` is not represented by the `a` type parameter.       |
| `Map.fromList [("1", "A"), ("2", "B")] :: Map Text Text` | `[ "A", "B" ]`            | The map keys are not represented by the `a` type parameter. |
|----------------------------------------------------------|---------------------------|-------------------------------------------------------------|

Notice that we're concerned only with parts of the `container item` that are represented by `item`. In the last example, the type in question is `Map key item`, which is specialized to `Map Text Text`. Both the keys (`key`) and the values (`item`) are text values, but the list we're interested in is only the items. We aren't going to invite the keys to the item just because in this case `key` and `item` happen to both be `Text`.

### Exercise: Count the values

For each type constructor `container`, how many `item`s can a value of type `container item` contain?

Fill in the blanks in the table below. Your choices are:

- No items at all
- Exactly 1
- Exactly 2
- Either 0 or 1
- Any number of items

|------------------------------|-------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|
| Type constructor `container` | A value of type `container item` contains how many `item`s?             | Notes                                                                                         |
|------------------------------|-------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|
| `Maybe`                      | 0 or 1, because `Nothing` holds no items and `Just` holds a single item | [From `base`](https://hackage.haskell.org/package/base/docs/Data-Maybe.html#t:Maybe)          |
| `List`                       |                                                                         | `data List item = Nil | Cons item (List item)`                                                |
| `Name`                       |                                                                         | `data Name item = Anonymous | Name item`                                                      |
| `Numbered`                   |                                                                         | `data Numbered item = Numbered Natural item`                                                  |
| `Tuple a`                    |                                                                         | `data Tuple b item = Tuple b item`                                                            |
| `Pair`                       |                                                                         | `data Pair item = Pair item item`                                                             |
| `Const a`                    |                                                                         | [From `base`](https://hackage.haskell.org/package/base/docs/Data-Functor-Const.html)          |
| `Set`                        |                                                                         | [From `containers`](https://hackage.haskell.org/package/containers/docs/Data-Set.html)        |
| `Map key`                    |                                                                         | [From `containers`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html) |
| `Tree`                       |                                                                         | [From `containers`](https://hackage.haskell.org/package/containers/docs/Data-Tree.html)       |
|------------------------------|-------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|

## Foldable: Working with list-like structures

All of the type constructors listed above belong to the `Foldable` class (or they could, if we add `deriving stock Foldable` to the definition as done below).

```haskell
data List item = Nil | Cons item (List item)
    deriving stock (Show, Foldable)

data Name item = Anonymous | Name item
    deriving stock (Show, Foldable)

data Numbered item = Numbered{ ordinal :: Natural, numberedItem :: item }
    deriving stock (Show, Foldable)

data Tuple b item = Tuple b item
    deriving stock (Show, Foldable)

data Pair item = Pair item item
    deriving stock (Show, Foldable)
```

### toList

Of the methods in the `Foldable` class, there is one of particular interest:

```haskell
toList :: Foldable container => container item -> [item]
```

This does exactly what we have been discussing: It pulls out all the `item` values.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> toList (Anonymous :: Maybe Text)
[]
ghci> toList (Just "Alonzo" :: Maybe Text)
["Alonzo"]
ghci> toList ("One" :: Text, 1)
[1]
ghci> toList (Pair 1 2)
[1,2]
ghci> toList (Node 1 [Node 2 [], Node 3 []])
[1,2,3]
```

The `Foldable` class is, in concept, quite simple: The `toList` method is its essence. It is a class for type constructors `container` where, given a value of type `container item`, we can obtain a list of its `item` values. This class, therefore, helps us write polymorphic functions that work over any list-like data structure.

### Generalizing a list parameter

Any function that has a list parameter can be converted into a more general function that has a `Foldable` constraint. For example, consider this procedure for dividing a list in half.

```haskell
everyOther :: [item] -> ([item], [item])
everyOther list =
    case list of
        [] -> ([], [])
        x : xs -> let (a, b) = everyOther' xs in (x : a, b)

everyOther' :: [item] -> ([item], [item])
everyOther' list =
    case list of
        [] -> ([], [])
        x : xs -> let (a, b) = everyOther xs in (a, x : b)
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> everyOther [1 .. 10]
([1,3,5,7,9],[2,4,6,8,10])
```

We can generalize `everyOther` by replacing `[item]` with `container item` and adding a `Foldable` constraint on the new `container` type parameter.

```haskell
everyOther :: Foldable container => container item -> ([item], [item])
```

The definition of this function can be made to support the new type signature by converting the `container item` argument to `[item]` using `toList` and then proceeding as before.

```haskell
everyOther foldable =
    let list = toList foldable in
    case list of
        [] -> ([], [])
        x : xs -> let (a, b) = everyOther' xs in (x : a, b)
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> everyOther (Set.fromList [1 .. 10])
([1,3,5,7,9],[2,4,6,8,10])
ghci> everyOther (Pair 1 2)
([1],[2])
ghci> numberWords = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
ghci> everyOther numberWords
([(1,"one"),(3,"three")],[(2,"two"),(4,"four")])
ghci> everyOther (Map.fromList numberWords)
(["one","three"],["two","four"])
```

This is not especially useful; the change we have made affords at best some minor convenience for users of the `everyOther` function. The `Foldable` class gets more interesting, however, when we start to look at some of its other methods. When we look at the details of the class, we find quite a lot:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :info Foldable
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
```

This is because the `[]` type is only one of many possible representations of a list, and it is not the most efficient one for all purposes. We do not, therefore, want to begin every operation on a list-like type by converting it to `[]`. The next example will illustrate more concretely why not.

### List operations, but faster

There are some things that the list type `[]` can do but isn't very good at. Consider the following function which produces a sort of summary of a list.

```haskell
summarize :: (Show item, Ord item) => [item] -> Text
summarize list =
    case list of
        [] -> "There are no values"
        _  -> Text.Builder.toLazyText $
              "There are " <> Text.Builder.decimal (length list) <>
              " values ranging from " <> Text.Builder.fromString (show (minimum list)) <>
              " to " <> Text.Builder.fromString (show (maximum list))
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> summarize ("kazoo" :: [Char])
"There are 5 values ranging from 'a' to 'z'"
```

We have used three operations here - `length`, `minimum`, and `maximum` -- that are linear-time operations on lists; each requires looking at the entire list. The `summarize` function is really better suited for a type of container like [`Set`](https://hackage.haskell.org/package/containers/docs/Data-Set.html). Here is how we would write the same function over sets instead of lists:

```haskell
summarize :: (Show item, Ord item) => Set item -> Text
summarize set =
    case Set.null set of
        True -> "There are no values"
        False -> Text.Builder.toLazyText $
              "There are " <> Text.Builder.decimal (Set.size set) <>
              " values ranging from " <> Text.Builder.fromString (show (Set.findMin set)) <>
              " to " <> Text.Builder.fromString (show (Set.findMax set))
```

For large inputs, the `Set` version is much faster because this data structure is optimized for these particular sorts of queries. Minimum and maximum lookups are logarithmic-time operations on a `Set`. The `Set.size` function is a constant-time operation because the `Set` type stores its size in a constructor field; it doesn't have to count through the whole collection every time you want to know how many items there are.

So the `Set` version of the `summarize` function is an improvement over the `[]` version. But we don't have to choose to write one or the other; we can write both at once. That's polymorphism, baby.

```haskell
summarize :: (Foldable container, Show item, Ord item) => container item -> Text
summarize foldable =
    case null foldable of
        True -> "There are no values"
        False -> Text.Builder.toLazyText $
              "There are " <> Text.Builder.decimal (length foldable) <>
              " values ranging from " <> Text.Builder.fromString (show (minimum foldable)) <>
              " to " <> Text.Builder.fromString (show (maximum foldable))
```

Using type applications, we can ask GHCi to confirm that the new more polymorphic type can specialize to either of the foldable containers we dealt with previously.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :set -XTypeApplications
ghci> :type summarize @[]
summarize @[] :: (Show item, Ord item) => [item] -> Text
ghci> :type summarize @Set
summarize @Set :: (Show item, Ord item) => Set item -> Text
```

<!-- A full discussion of list access patterns, particularly of foldr and foldl', belongs somewhere in the "control structures" or "data structures" lesson, or somewhere thereabouts, because these functions should be studied specifically for the [] type constructor before trying to understand how they are generalized in Foldable. When that exists, we should link back to it from here. -->

#### Exercise: Specialization

Each of the following functions from `Prelude` has a `Foldable` constraint.

|------------------------------------------------------------------------------------|--------------------------------------------|------------------------------------------------------------------------------------------|
| Function                                                                           | Type                                       | Description                                                                              |
|------------------------------------------------------------------------------------|--------------------------------------------|------------------------------------------------------------------------------------------|
| [`all`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:all)          | `Foldable t => (a -> Bool) -> t a -> Bool` | Determines whether all elements of the structure satisfy the predicate.                  |
| [`concat`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:concat)    | `Foldable t => t [a] -> [a]`               | The concatenation of all the elements of a container of lists.                           |
| [`concatMap`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:concat) | `Foldable t => (a -> [b]) -> t a -> [b]`   | Map a function over all the elements of a container and concatenate the resulting lists. |
|------------------------------------------------------------------------------------|--------------------------------------------|------------------------------------------------------------------------------------------|

What types do the following specializations have? Fill in the blanks in the table below.

|-------------------------|--------------------------------|
| Expression              | Type                           |
|-------------------------|--------------------------------|
| `all @Set`              | `(a -> Bool) -> Set a -> Bool` |
| `all @[]`               |                                |
| `concat @Set`           |                                |
| `concat @[]`            |                                |
| `concatMap @(Map Text)` |                                |
|-------------------------|--------------------------------|

Given the definitions below, what is the type of `result`? What is its value?

```haskell
numberWords :: Map Text Text
numberWords = Map.fromList [("1", "one"), ("2", "two")]

result = concatMap (\x -> [x, "!"]) numberWords
```

### What Foldable can't do

The `Foldable` class can only be used to generalize a function's parameter type; it has no ability to generalize a function's return type. This is because the class offers only means of *consuming* values of the `Foldable` type, not any means of *constructing* values of that type. For example, the following generalization of [`filter`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:filter) cannot be written:

```haskell
filter :: Foldable container =>
    (item -> Bool) -> container item -> container item
```

A polymorphic `filter` function is available in the [witherable](https://hackage.haskell.org/package/witherable/docs/Witherable.html) library, which introduces a different class (`Filterable`) specifically for this purpose.

The [ListLike](https://hackage.haskell.org/package/ListLike/docs/Data-ListLike.html) library offers a more general coverage of the majority of operations one might want to perform on list-like data structures. It too provides a [`filter`](https://hackage.haskell.org/package/ListLike/docs/Data-ListLike.html#v:filter) function, and much more.

### What types are Foldable

All of the type constructors listed at the beginning of this lesson, and many more, belong to the `Foldable` class. If a datatype definition `t` has a type parameter and `t a` has an `Eq` instance, then `t` can probably have a `Foldable` instance as well.

The most prominent examples of types that do not belong to the class are, as usual, functions `IO` and `(->)`. Think about what it would mean for `IO` to belong to the `Foldable` class. What would we want `toList :: IO a -> [a]` to do? Perhaps it should enumerate all of the `a` values that the I/O action might produce as its result. For some particular actions, this might be knowable. In general, however, it is not.

### Writing a custom Foldable instance

All of the `Foldable` examples we have seen so far can be described as "containers"; the items produced in the result of `toList` appear as fields contained within the foldable datatype. This is the most common situation. For the sake of broadening our worldview, we can concoct some other valid examples. To construct some atypical instances, we will have to write them ourselves instead of deriving.

A minimal complete definition for `Foldable` requires implementing either `foldr` or `foldMap`.

```haskell
foldr   :: Foldable container             => (item -> s -> s) -> s -> container item -> s
foldMap :: (Foldable container, Monoid s) => (item -> s)           -> container item -> s
```

Both are methods that for rolling up the item list into some summary value (type parameter `s` above); they differ only in the details of how the rolling-up is expressed.

#### A standard instance

Before we get to atypical instances, let's start by considering how to write a `Foldable` that is equivalent to the stock instance.

```haskell
data Triple item = Triple item item item
```

With the `foldr` approach, we are given a starting value (`s`) and function (`item -> s -> s`) which in some way contributes a new item to the result. Our implementation of `foldr` must proceed from the given start value and then repeated apply the function to incorporate each item into the snowballing accumulation of items.

```haskell
instance Foldable Triple where
    foldr incorporate start (Triple item1 item2 item3) =
        start & incorporate item1 & incorporate item2 & incorporate item3
```

For fans of `Monoid`, the `foldMap` method may be an easier approach than `foldr`. With this method, we are given only a function that converts from the `item` type to the result type, but we are also assured that this type belongs to the `Monoid` class. Our implementation of `foldMap` must apply the conversion function to each item, and then monoidally combine the results. In other words: It should *map* the function over the item list, then *fold* the results.

For our `Triple` type, an instance based on `foldMap` looks like this:

```haskell
instance Foldable Triple where
    foldMap f (Triple item1 item2 item3) =
        f item1 <> f item2 <> f item3
```

As mentioned above, we would not actually write this for the `Triple` type; we would use the stock-derived instance instead.

```haskell
data Triple item = Triple item item item
    deriving stock Foldable
```

Next we will deviate from the stock behavior.

#### Repetitions

Suppose we have some situation where we're often working with lists that contain long series of repeated elements, like "aaaaabbbcccccccccccccccccccc". We might want to store such a list in compressed form like `[(5, 'a'), (3, 'b'), (20, 'c')]`. Here is one idea of what that might look like:

```haskell
newtype ReplicaSeq a = ReplicaSeq (Seq (Int, a))
    deriving stock Show
    deriving newtype (Semigroup, Monoid)

rep :: Int -> a -> ReplicaSeq a
rep n x = ReplicaSeq (Seq.singleton (n, x))

example :: ReplicaSeq Char
example = rep 5 'a' <> rep 3 'b' <> rep 20 'c'
```

The `example` sequence contains the three `Char` values `'a'`, `'b'`, and `'c'`. However, this `ReplicaSeq` is meant to represent a list of 28 values. If we want a `Foldable` instance that respects this meaning, then we'll have to write it ourselves.

```haskell
instance Foldable ReplicaSeq where
    foldMap f (ReplicaSeq xs) =
        foldMap (\(n, x) -> mtimesDefault n (f x)) xs
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> toList (rep 5 'a' <> rep 3 'b' <> rep 20 'c')
"aaaaabbbcccccccccccccccccccc"
```

#### Functions

We mentioned that functions are not, in general, foldable. However, if we bring back the `FiniteFunction` newtype that we introduced in the `Show` lesson, we can write a `Foldable` instance for functions that relies on being able to enumerate the function's domain.

```haskell
newtype FiniteFunction a b = FiniteFunction (a -> b)

instance (Enum domain, Bounded domain) => Foldable (FiniteFunction domain) where
    foldMap f (FiniteFunction g) = foldMap (f . g) [minBound .. maxBound]

fiftyIfTrue :: Bool -> Integer
fiftyIfTrue x = case x of
    True -> 50
    False -> 0
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> length (FoldableFunction fiftyIfTrue)
2
ghci> toList (FoldableFunction fiftyIfTrue)
[0,50]
```

This is another example that stretches our understanding of foldability beyond what we normally think of as "containers".

#### Exercise: Predicates

Consider the following datatype, which can be found in the [`Data.Functor.Contravariant`](https://hackage.haskell.org/package/base/docs/Data-Functor-Contravariant.html) module of the `base` package.

```haskell
newtype Predicate a = Predicate (a -> Bool)
```

Write a function with the following type signature that produces a list of all values that satisfy the predicate.

```haskell
predicateToList :: (Enum a, Bounded a) => Predicate a -> [a]
```

For example, it should behave as follows:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> predicateToList (Predicate (\x -> x > 200 && rem x 5 == 0) :: Predicate Word8)
[205,210,215,220,225,230,235,240,245,250,255]
```

Since we can convert predicates to lists, it might seem that we could write a `Foldable` instance for the `Predicate` type. However, this is not possible. Why not?

## Functor

`Functor` is another class of type constructors. Its definition is rather minimal, buts its consequences are surprisingly far-reaching.

```haskell
class Functor container where
    fmap :: (item1 -> item2) -> container item1 -> container item2
```

(The 'f' in 'fmap' stands for 'Functor'.)

This says that if something is a `Functor`, then we can apply a function to each of its items. In the examples below, the item type is `Text`, and the function we map over the container is `exclaim`.

```haskell
exclaim :: Text -> Text
exclaim x = x <> "!"
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> fmap exclaim ["Go", "Fight", "Win"]
["Go!","Fight!","Win!"]
ghci> fmap exclaim (Map.fromList [("1", "one"), ("2", "two")] :: Map Text Text)
fromList [("1","one!"),("2","two!")]
```

Recall that the `foldMap` function also allowed us to apply a function to each item. The difference between `fmap` and `foldMap` is in the type of result we get back.

```haskell
foldMap :: (Foldable container, Monoid b) => (a -> b) -> container a -> b
fmap    :: (Functor container)            => (a -> b) -> container a -> container b
--                                                                      ^^^^^^^^^^^
```

`Foldable` operations discard details about the data structure. For example, below we consider two trees that contain the same elements in different arrangements.

```haskell
tree1, tree2 :: Tree String
tree1 = Node "a" [Node "b" [], Node "c" []]
tree2 = Node "a" [Node "b" [Node "c" []]]
```

We can use the [`drawTree`](https://hackage.haskell.org/package/containers/docs/Data-Tree.html#v:drawTree) function to see the tree structures more clearly.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> putStrLn $ Tree.drawTree tree1
a
|
+- b
|
`- c
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> putStrLn $ Tree.drawTree tree2
a
|
`- b
   |
   `- c
```

In the eyes of `Foldable`, there is no distinction between `tree1` and `tree2`. The only thing that matters is the items and what linear order they come out in.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> toList tree1
["a","b","c"]
ghci> toList tree2
["a","b","c"]
```

A `Functor` operation, on the other hand, doesn't destroy the container. Moreover, it does not modify the shape of the tree in any way.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> putStrLn $ Tree.drawTree $ fmap (\x -> x <> x) tree1
aa
|
+- bb
|
`- cc
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> putStrLn $ Tree.drawTree $ fmap (\x -> x <> x) tree2
aa
|
`- bb
   |
   `- cc
```

The resulting trees now contain different values, but they are otherwise the same as the trees we started with.

Furthermore, `fmap` allows us to change the item type, with a consequent change to the container type. For example, it we change a list's item type from `Natural` to `Text`, then the list type changes from `[Natural]` to `[Text]`.

```haskell
ghci> fmap (Text.Builder.toLazyText . Text.Builder.decimal) ([1, 3, 5] :: [Natural])
["1","3","5"]
```

This is quite an extraordinary bit of polymorphism. This one `fmap` operation involves four types: `Natural`, `Text`, `[Natural]`, and `[Text]`.

```haskell
fmap :: (item1 -> item2) -> container item1 -> container item2
--      Natural   Text        [Natural]            [Text]
```

Although we have been using the terms *container* and *item* here, the type of `fmap` is more commonly written tersely as:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

We must soon let go of the *container*/*item* metaphor anyway, because it will start to feel less apt when we get to the `IO` functor.

### Exercise: Identify the types

In the example, below, what are the concrete specializations of each of the parts of the `fmap` type signature? Fill in the blanks in the table.

```haskell
constants :: Map Text Double
constants = Map.fromList [("Pi", 3.14159), ("Euler's number", 2.71828), ("Golden ratio", 1.618034)]
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> fmap (round :: Double -> Integer) constants
fromList [("Euler's number",3),("Golden ratio",2),("Pi",3)]
```

|----------|----------|
| Abstract | Concrete |
|----------|----------|
| `a`      | `Double` |
| `b`      |          |
| `f`      |          |
| `f a`    |          |
| `f b`    |          |
|----------|----------|

### Which types are Functors

If you're writing a datatype that has a type parameter, there's an excellent chance that you can (and should) give it a stock-derived `Functor` instance. Any time you have a container that can hold any sort of value whatsoever, more likely than not at some point you'll want to map some function over the contained values. Let's add it to the list of derived classes for all of the datatypes we defined earlier.

```haskell
data List item = Nil | Cons item (List item)
    deriving stock (Show, Foldable, Functor)

data Name item = Anonymous | Name item
    deriving stock (Show, Foldable, Functor)

data Numbered item = Numbered{ ordinal :: Natural, numberedItem :: item }
    deriving stock (Show, Foldable, Functor)

data Tuple b item = Tuple b item
    deriving stock (Show, Foldable, Functor)

data Pair item = Pair item item
    deriving stock (Show, Foldable, Functor)
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> fmap exclaim Anonymous
Anonymous
ghci> fmap exclaim (Name "Me")
Name "Me!"
ghci> fmap (+ 1) (Pair 10 20)
Pair 11 21
ghci> fmap (+ 1) (Tuple 10 20)
Tuple 10 21
```

There is (with very small exception) no reason to ever write a `Functor` instance manually, because the stock instance is the only sensible way to define `fmap`. The `Functor` class has some requirements that state this more formally, and it can be shown that there is at most one valid implementation of `fmap` for any type constructor. There really is no room to play or get creative here.

There are so many functors, it is more fruitful to discuss what sorts of type constructors are *not* functors.

#### Contravariance

Consider the following types from the [`Data.Functor.Contravariant`](https://hackage.haskell.org/package/base/docs/Data-Functor-Contravariant.html) module.

```haskell
newtype Predicate item =
    Predicate { getPredicate :: item -> Bool }

newtype Op a item =
    Op { getOp :: item -> a }

newtype Equivalence item =
    Equivalence { getEquivalence :: item -> item -> Bool }

newtype Comparison item =
    Comparison { getComparison :: item -> item -> Ordering }
```

What do all of these type constructors -- `Predicate`, `Op a`, `Equivalence`, and `Comparison` -- have in common? The `item` type parameter appears to the left of a function arrow. These things are therefore not containers that *have* or *produce* items; they are functions that *require* or *accept* items. They're the opposite of a functor. None of these types can belong to the `Functor` class. (They belong to a different class called [`Contravariant`](https://hackage.haskell.org/package/base/docs/Data-Functor-Contravariant.html), with a reversed counterpart to `fmap` called `contramap`.)

#### Validity constraints

For another example of why we might not have a `Functor`, consider a new module:

```haskell
module SortedPair (SortedPair, fromPair, toPair) where

data SortedPair a = SortedPair{ smaller :: a, larger :: a }
    deriving stock (Eq, Ord, Show, Foldable)

fromPair :: Ord a => Pair a -> SortedPair a
fromPair (Pair a b) =
    if (a <= b) then (SortedPair a b) else (SortedPair b a)

toPair :: SortedPair a -> Pair a
toPair (SortedPair a b) = Pair a b
```

This module maintains the property that for any `SortedPair` value, the `smaller` field will never be greater than the `larger` field.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> fromPair (Pair 20 10)
SortedPair {smaller = 10, larger = 20}
```

Now suppose we were to introduce a `Functor` instance:

```haskell
data SortedPair a = SortedPair{ smaller :: a, larger :: a }
    deriving stock (Eq, Ord, Show, Foldable, Functor)
```

Using `fmap`, we are now able to construct a `SortedPair` that is not sorted. This `Functor` instance is valid in the sense that it obeys the rules of the `Functor` class, but it is invalid in the sense that it violates the rules of the `SortedPair` type.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> fmap (\x -> 100 - x) (fromPair (Pair 20 10))
SortedPair {smaller = 90, larger = 80}
```

Most of the `Foldable` type constructors discussed earlier, with one exception, are `Functor`s. The exception is `Set`; it lacks a `Functor` for much the same reason as `SortedPair`. The `Set` type does more than just blindly hold some items; it keeps them arranged in sorted order using the item type's `Ord` instance. If we were able to reach into sets and fiddle with their items in the way that `Functor` allows, we would destroy all the careful bookkeeping that `Set` has done for us and end up with sets that don't work as sets ought to.

There does actually exist a precarious function, called [`mapMonotonic`](https://hackage.haskell.org/package/containers/docs/Data-Set.html#v:mapMonotonic), that can do this! Use with caution.

```haskell
mapMonotonic :: (a -> b) -> Set a -> Set b
```

We can use it to do terrible things, such as to create a invalid `Set` that has two of the same item in it.

```haskell
someWords :: Set Text
someWords = Set.fromList ["Apple", "Banana", "Carpet", "Cartoon"]
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> Set.mapMonotonic (Text.take 3) someWords
fromList ["App","Ban","Car","Car"]
```

Technically, we could use this to define a `Functor` instance.

```haskell
instance Functor Set where
    fmap = Set.mapMonotonic
```

... but this would be an imprudent choice that will eventually lead to erroneous conclusions.

The `Set` type does have its own [`map`](https://hackage.haskell.org/package/containers/docs/Data-Set.html#v:map) function that is roughly analogous to `fmap` and behaves in a sensible way.

```haskell
Set.map :: Ord b => (a -> b) -> Set a -> Set b
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> Set.map (Text.take 3) someWords
fromList ["App","Ban","Car"]
```

`Set.map` does not meet the criteria for being a definition of `fmap`, however, because it has an `Ord` constraint on the resulting item type, and `fmap` does not. If we try to define the `Functor` instance in this way, the compiler tells us as much.

```haskell
instance Functor Set where
    fmap = Set.map
```

```default
error:
    • No instance for (Ord b) arising from a use of ‘Set.map’
```

The lesson here is that just because a function doesn't perfectly meet the criteria for belonging to a typeclass doesn't mean we can't still have that function; we just can't call it `fmap`.

We have now seen the two major reasons why a type constructor would not have a `Functor` instance:

  1. If the type parameter appears as a function argument (in which case it is said to appear *in contravariant position*, to the left of a function arrow), then implementing `fmap` is not possible.
  2. If `fmap` is possible but wouldn't satisfy the particular properties that a type's author wants to enforce, such as in the case of the `SortedPair` or `Set` types, then a `Functor` instance is undesirable.

#### IO

As we have just seen, not all foldables are functors, because `Functor` permit us to change the item type freely, and it does not allow the container to impose any constraints on that transformation. For example, `Set` only works with `Ord` items, and this limitation disqualifies it as a functor.

Conversely, not all functors are foldable. This is because `Foldable` requires a little something extra of the relationship between the container and item: The container must be able to produce a list of the items, and not all functors can do that. For example, an `IO` action does not "contain" items in this way. It does, however, belong to the `Functor` class.

Specialized to `IO`, the type of `fmap` is:

```haskell
fmap @IO :: (a -> b) -> IO a -> IO b
```

This means that given any `IO` action, we can `fmap` a function over it to modify its result. For example, consider the [`getEnvironment`](https://hackage.haskell.org/package/base/docs/System-Environment.html#v:getEnvironment) action, which returns a list of all the [environment variables](https://en.wikipedia.org/wiki/Environment_variable) of the current process.

```haskell
getEnvironment :: IO [(String, String)]
```

You can run this in GHCi, and it will print a lot of information.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> getEnvironment
...
```

Suppose we are only interested in knowing how many variables there are in the system environment. We can `fmap` the `length` function over this action. (The result you see will vary.)

```console?lang=haskell&prompt=ghci>,ghci|
ghci> fmap length getEnvironment
87
```

##### Exercise: Types of actions

The type of `length` above is `[(String, String)] -> Int`. What is the type of `fmap length getEnvironment`?

The following `getEnvironmentNames` action returns only the names of the environment variables, discarding their values.

```haskell
getEnvironmentNames :: IO [String]
getEnvironmentNames = fmap (fmap environmentName) getEnvironment

environmentName :: (String, String) -> String
environmentName (name, value) = name
```

The definition of `getEnvironmentNames` uses `fmap` twice. For each of the two, what is the concrete type of `fmap`?

### Functions with Functor constraints

All of these goodies may be found in the [`Data.Functor`](https://hackage.haskell.org/package/base/docs/Data-Functor.html) module: `(<$>)`, `(<&>)`, `(<$)`, `($>)`, and `void`. Each of these functions does very little (as you will see in the next exercise), but they offer some conveniences.

`(<$>)` and `(<&>)` are infix variations of `fmap` that come in handy when one of the arguments is a larger expression.

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<&>) :: Functor f => f a -> (a -> b) -> f b
```

The `(<&>)` function is the same as `fmap`, but with its parameter order reversed; this is often more convenient when the *function* argument is a lambda. This is what we see in the following example, where the result from `getEnvironment` is altered by restricting the list to a few particular environment variable names.

```haskell
getFilteredEnvironment :: IO [(String, String)]
getFilteredEnvironment = getEnvironment <&> \vars ->
    filter (\(name, _) -> elem name ["USER", "SHELL"]) vars
```

The `(<$>)` function is simply a synonym for `fmap`. It is often convenient when the *functor* argument is a `case` or `do` expression. In the next example, we define another action that gives the number of variables in the environment, but this time it is parameterized on whether we want to consider the entire environment or only its filtered subset.

```haskell
data EnvironmentType = Entire | Filtered

getEnvironmentSize :: EnvironmentType -> IO Int
getEnvironmentSize =
    length <$> case config of
        Entire   -> getEnvironment
        Filtered -> getFilteredEnvironment
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> getEnvironmentSize Entire
87
ghci> getEnvironmentSize Filtered
2
```

`(<$)`, `($>)`, and `void` are not often very useful for container data structures; they serve only to obliterate the items within the functor.

```haskell
(<$) :: Functor f => b -> f a -> f b
($>) :: Functor f => f a -> b -> f b
void :: Functor f => f a -> f ()
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> "ho" <$ [1,2,3]
["ho","ho","ho"]
ghci> constants $> 0
fromList [("Euler's number",0),("Golden ratio",0),("Pi",0)]
ghci> void [1,2,3,4,5]
[(),(),(),(),()]
```

Notice, however, that although the original items are lost, the data structure still maintains its original shape. Each resulting list has the same length as the list that we started with, and the `Map Text` still has the same keys.

The utility of these functions, and more about the importance of `Functor` itself, will come to light in the next lesson on `Applicative` and `Monad`.

#### Exercise: Functor functions

Define each of the following functions, using only `fmap`.

|----------|---------------------------------------|-------------------------------|
| Function | Type                                  | Definition                    |
|----------|---------------------------------------|-------------------------------|
| `(<$>)`  | `Functor f => (a -> b) -> f a -> f b` | `(<$>) = fmap`                |
| `(<&>)`  | `Functor f => f a -> (a -> b) -> f b` |                               |
| `(<$)`   | `Functor f => b -> f a -> f b`        | `b <$ fa = fmap (\_ -> b) fa` |
| `($>)`   | `Functor f => f a -> b -> f b`        |                               |
| `void`   | `Functor f => f a -> f ()`            |                               |
| ---------|---------------------------------------|-------------------------------|

### Multiple layers of type constructor

When one functor is nested within another, sometimes the function argument to `fmap` involves a second `fmap`.

```haskell
numberSequences :: Seq (Seq Integer)
numberSequences = Seq.fromList [Seq.fromList [1, 2, 3], Seq.fromList [4, 5, 6]]
```

When we apply `fmap` to `numberSequences`, the function argument receives a `Seq Integer` as its argument. We can use this to modify each `Seq` in the `Seq` -- for example, to shorten each `Seq` by one.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> fmap (Seq.drop 1) numberSequences
fromList [fromList [2,3],fromList [5,6]]
```

Support we want to apply a function to each integer within each `Seq`:

```haskell
nestedDouble :: Seq (Seq Integer) -> Seq (Seq Integer)
nestedDouble = fmap (fmap (* 2))
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> nestedDouble numberSequences
fromList [fromList [2,4,6],fromList [8,10,12]]
```

This is the sort of situation where the `(<&>)` operator might be preferable.

```haskell
nestedDouble :: Seq (Seq Integer) -> Seq (Seq Integer)
nestedDouble nestedSequence =
    nestedSequence <&> \sequence ->  -- over each (Seq Integer) ...
    sequence <&> \number ->          -- over each Integer ...
    number * 2                       -- double it
```

The `Functor` class is a central idea that many other constructor classes build upon. The next two lessons cover `Applicative`, `Monad`, and `Traversable`; all three are subclasses of `Functor`.
