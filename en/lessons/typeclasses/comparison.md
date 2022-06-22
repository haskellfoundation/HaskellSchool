---
version: "1.0.0"
title: "Comparing values: Eq and Ord"
---

{% include toc.html %}

In this lesson, we use the [`text`](https://hackage.haskell.org/package/text) and [`containers`](https://hackage.haskell.org/package/containers) packages. We also require the following language extensions:

```haskell
{-# language DerivingStrategies, OverloadedStrings #-}
```

## Eq: Distinguishable values

Two values of the same type may be tested for equality using `(==)`:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 'A' == 'A'
True
ghci> 'A' == 'B'
False
```

`(==)` is a method of the [`Eq`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Eq) class.

### Types in the Eq class

The `Eq` class includes most basic datatypes, such as [`Bool`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Bool), [`Char`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Char) and [`Integer`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Integer).

The `Eq` class also includes more complex datatypes, as long as they are made up of types that belong to the `Eq` class. For example, because integers can be tested for equality, lists of integers (`[Integer]`) can as well.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> [1, 2, 3] == [1, 2, 4]
False
```

#### Types not in the Eq class

Not everything can be tested for equality. For example, the `Eq` class does not include functions. So if we try to compare two functions, what we get is a compile error:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> (\x -> x + 1 + 1) == (\x -> x + 2)

error:
    • No instance for (Eq (Integer -> Integer))
```

Although in this case we can see that these two functions are the same (they both add two to a number), this determination is not always possible to make. So, `(==)` is not defined for functions, and thus function types such as `Integer -> Integer` do not belong to the `Eq` class.

[`IO`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:IO) actions also do not have a meaningful equality test. Again we see compilation failure when we try to use the `(==)` operation with a type that does not support it:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> putStrLn "hello" == writeFile "result.txt" "okay"
error:
    • No instance for (Eq (IO ())) arising from a use of ‘==’
```

### Deriving Eq for new types

Suppose we define a `Message` as a record with two [`Text`](https://hackage.haskell.org/package/text/docs/Data-Text-Lazy.html#t:Text) fields:

```haskell
import Data.Text (Text)
import qualified Data.Text as Text

data Message = Message { subject :: Text, body :: Text }

message1 = Message{ subject = "Hi", body = "How are you"}
message2 = Message{ subject = "Hi", body = "How are you???"}
```

The `Text` type belongs to the `Eq` class. This lets us test the two messages' subjects and bodies for equality.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> subject message1 == subject message2
True
ghci> body message1 == body message2
False
```

We might also want to ask whether the two messages are equal. However, we cannot yet, because the `(==)` function is not yet defined for our `Message` type:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> message1 == message2
error:
    • No instance for (Eq Message) arising from a use of ‘==’
```

We do not need to define `(==)` explicitly, because the compiler can generate it for us. Modify the definition of the `Message` datatype by adding an additional clause that begins with the `deriving` keyword:

```haskell
data Message = Message { subject :: Text, body :: Text }
    deriving stock Eq
```

This `Message` type now has an instance of `Eq`, and so its values can be tested for equality.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> message1 == message2
False
ghci> message1 /= message2
True
```

Obtaining an `Eq` instance in this way is called "stock deriving" because it takes advantage of the compiler's built-in support for deriving the `Eq` class.

#### When Eq cannot be derived

To derive a stock `Eq` instance for a record like `Message`, all of the constructor's fields must have `Eq` instances. Suppose our type contains an additional field, an `IO` action which sends the message somewhere.

```haskell
data Message = Message { subject :: Text, body :: Text, send :: IO () }
    deriving stock Eq
```

Since this third field cannot be tested for equality, the compiler cannot derive `Eq` for the `Message` type, and it will tell us so as the compilation fails.

```
error:
    • No instance for (Eq (IO ()))
        arising from the third field of ‘Message’ (type ‘IO ()’)
```

### Some functions constrained by Eq

#### Not-equals (/=)

The opposite of `(==)` is `(/=)`:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 1 == 2
False
ghci> 1 /= 2
True
```

This function doesn't do much; it is merely the negation of `(==)`.

```haskell
(/=) :: Eq a => a -> a -> Bool
x /= y = not (x == y)
```

#### group

Many of the general-purpose functions in the [`Data.List`](https://hackage.haskell.org/package/base/docs/Data-List.html) module involve an `Eq` constraint. The [`group`](https://hackage.haskell.org/package/base/docs/Data-List.html#v:group) function detects when a list contains consecutive repetitions. The repeated elements are collected together in the output.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> group (Text.words "Text has an an Eq instance")
[["Text"],["has"],["an","an"],["Eq"],["instance"]]
```

To do this, it applies `(==)` to each pair of neighbors in the list. Therefore the `group` function can only be used on lists where the elements' type belongs to the `Eq` class. This is indicated by the type of `group`:

```haskell
group :: Eq a => [a] -> [[a]]
```

The type context of this function is `Eq a`, which means that in the type `[a] -> [[a]]`, the variable `a` must stand for some type with an instance of `Eq`, such as `Text`.

#### nub

The [`nub`](https://hackage.haskell.org/package/base/docs/Data-List.html#v:nub) function removes duplicates from a list, producing a resulting list in which no element appears more than once.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> nub [1,3,3,2,3,1,2]
[1,3,2]
```

This is another function that compares pairs of elements. `nub` performs more comparisons than `group` does, since `nub` has to consider all pairs of elements from the list, not merely the ones that appear side-by-side. But the type contexts are the same: Both functions require `a` to be something that can be tested for equality.

```haskell
nub :: Eq a => [a] -> [a]
```

(When we discuss the `Ord` class below, we will see a faster alternative to `nub` that requires comparisons of fewer element pairs.)

##### Exercise: No instance ... arising from a use of ...

The following code does not compile. Try to compile it, and read the ensuing error message.

```haskell
import qualified Data.List as List

numberOfDistinctElements :: [a] -> Int
numberOfDistinctElements xs = length (List.nub xs)
```

What is the cause of the problem? How can you fix it? (Read the error message!)

#### lookup

The `lookup` function searches through a list of `(a, b)` tuples (for any `a` and `b`) to find a particular `a` value, and then returns the `b` value that is associated with it.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> numberWords = [(1, "One"), (5, "Five"), (7, "Seven")] :: [(Integer, Text])]
ghci> lookup 5 numberWords
Just "Five"
ghci> lookup 6 numberWords
Nothing
```

##### Exercise: What is the polymorphic type?

The concrete type of the `lookup` function, as we used it in the context above, is:

```haskell
lookup :: Integer -> [(Integer, Text)] -> Maybe Text
```

What is the more general polymorphic type of `lookup`? First see if you can figure it out for yourself. Are there any constraints? Then enter `:type lookup` into GHCi to check.

### Writing Eq instances

The default behavior of `(==)` that we get from deriving `Eq` is usually appropriate, but you may also write a custom `Eq` instance to design an equality test that disregards some aspects of the compared values. These sorts of tricks are uncommon.

#### Read the class definition

The first thing we need to do when writing a typeclass instance is to look at the definition of the class. This tells us what methods the new instance will need to provide definitions for. Use the `:info` command in GHCi to inspect the class:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :info Eq
```

The output includes, among other things, the `class` definition:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

This class has two methods: `(==)` and `(/=)`. The `MINIMAL` pragma informs us that an instance must provide a definition for either `(==)` or `(/=)`.

#### Case-insensitive text

In this following example, we define a datatype called `CaseInsensitiveText`. It is like `Text`, but with an equality test that disregards differences in capitalization of letters.

```haskell
import Data.Lazy (Text)
import qualified Data.Lazy as Text

data CaseInsensitiveText = CaseInsensitiveText{ caseSensitiveText :: Text }

instance Eq CaseInsensitiveText
  where
    x == y =
        Text.toLower (caseSensitiveText x) ==
        Text.toLower (caseSensitiveText y)
```

The `instance` definition above specifies what `x == y` means when `x` and `y` are values of the `CaseInsensitiveText` type. Two `CaseInsensitiveText` values are considered to be equal if the lower-case forms of their `Text` values are equal.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> CaseInsensitiveText "HELLO" == CaseInsensitiveText "hello"
True
```

#### Containers

Sophisticated container types -- such as [`Map`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#t:Map), [`Set`](https://hackage.haskell.org/package/containers/docs/Data-Set.html#t:Set), and [`Seq`](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html#t:Seq) in the `containers` package -- make use of custom `Eq` instances when an exact comparison of the containers' internal structure might reveal details that users of the containers are not meant to perceive.

For a small demonstration, suppose we were not using the `containers` package, but we wanted to write our own simple `Set` type intead. A "set" here is defined as a list in which repetitions and ordering are disregarded.

```haskell
import Data.List (nub)

data Set a = Set{ list :: [a] }

instance Ord a => Eq (Set a)
  where
    x == y =
        nub (sort (list x)) ==
        nub (sort (list y))
```

By applying [`sort`](https://hackage.haskell.org/package/base/docs/Data-List.html#v:sort) and [`nub`](https://hackage.haskell.org/package/base/docs/Data-List.html#v:nub), any irrelevant differences in order and number are ironed out of the lists before we compare them.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> Set [1,3,2,2] == Set [3,2,1]
True
```

Even though `[1,3,2,2]` and `[3,2,1]` are different lists, the `Eq` instance defined above expresses our intent to treat them as the same in the context of the `Set` type.

## Ord: Sortable values

The [`Ord`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Ord) class expands upon `Eq` by introducing another method: `compare`.

Whereas `(==)` returns one of two possibilities (`True` for equality and `False` for inequality), `compare` can give one of [three results](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Ordering): `LT`, `EQ`, or `GT`.

|-----------|-----------------------|-------------------------------------|
| When ...  | What `Eq` says        | What `Ord` says                     |
|-----------|-----------------------|-------------------------------------|
| x < y     | `x == y` = `False`    | `compare x y` = `LT` (less than)    |
| x = y     | `x == y` = `True`     | `compare x y` = `EQ` (equal)        |
| x > y     | `x == y` = `False`    | `compare x y` = `GT` (greater than) |
|-----------|-----------------------|-------------------------------------|

```console?lang=haskell&prompt=ghci>,ghci|
ghci> compare 3 5
LT
ghci> compare 5 5
EQ
ghci> compare 7 5
GT
```

The `compare` function is great for situations where you want to handle each of these three cases separately.

```haskell
bottomLine :: Rational -> Text
bottomLine profit =
    case (compare profit 0) of
        GT -> "We made a profit!"
        EQ -> "We broke even."
        LT -> "We are not doing well."
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> bottomLine 34.89
"We made a profit!"
ghci> bottomLine (-112.34)
"We are not doing well."
```

### Some functions constrained by Ord

#### Predicates: (<), (<=), (>), and (>=)

When you do not care to handle all three cases, there are more convenient ways to compare.

|----------|--------------------------|
| Operator | Pronunciation            |
|----------|--------------------------|
| `(<)`    | Less than                |
| `(<=)`   | Less than or equal to    |
| `(>)`    | Greater than             |
| `(>=)`   | Greater than or equal to |
|----------|--------------------------|

```console?lang=haskell&prompt=ghci>,ghci|
ghci> 6 < 6
False
ghci> 6 <= 6
True
```

#### min and max

`max` selects the greater of two values, and `min` selects the lesser.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> max (-4) 0
0
ghci> min 5 200
5
```

#### sort

If we can compare two values, then we can sort lists. In the `Data.List` module, there is the aptly-named [`sort`](https://hackage.haskell.org/package/base/docs/Data-List.html#v:sort) function:

```console?lang=haskell&prompt=ghci>,ghci|
ghci> sort [1,3,6,4,3,2,4,1]
[1,1,2,3,3,4,4,6]
ghci> sort "applesauce"
"aaceelppsu"
```

#### Set and Map operations

The [`Set`](https://hackage.haskell.org/package/containers/docs/Data-Set.html) and [`Map`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html) types in the `containers` library permit a number of efficient operations by maintaining collections of values that are stored always in sorted order. Many of the functions that do manipulations and lookups on these types therefore have an `Ord` constraint, because they need to make use of the `compare` function to maintain order.

```haskell
-- | Create a set from a list of elements.
fromList :: Ord a => [a] -> Set a

-- | Is the element in the set?
member :: Ord a => a -> Set a -> Bool
```

The `Set` type, by taking advantage of the extra information provided by `Ord`, gives us a faster way to remove duplicates from a list than the `nub` function we used earlier.

```console?lang=haskell&prompt=ghci>,ghci|'
ghci> import qualified Data.Set as Set
ghci> Set.toAscList (Set.fromList [1,3,2,2])
[1,2,3]
```

##### Why arbitrary orderings are useful

Sometimes we give a type an instance of the `Ord` class even if the "ordering" is not meaningful. For example, [`ExitCode`](https://hackage.haskell.org/package/base/docs/System-Exit.html#t:ExitCode) has an `Ord` instance, even though it is perhaps not important to know that success is "less than" failure.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> import System.Exit
ghci> ExitSuccess < ExitFailure 1
True
```

These sorts of orderings are nevertheless useful because they help us construct sets and maps.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> import qualified Data.Set as Set
ghci> possibleExitCodes = Set.fromList [ExitSuccess, ExitFailure 1]
```

Were it not for the `Ord` instance on `ExitCode`, building a `Set` of `ExitCode` would not be possible.

### Types in the Ord class

Typically, a type with an `Eq` instances will have an `Ord` instance too. Most basic datatypes like `Char` and `Integer` belong to the `Ord` class.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> compare 'a' 'b'
LT
```

More complex datatypes belong to the `Ord` class as long as their parts belong to the `Ord` class. For example, since `Char` has an ordering, then `[Char]` has an ordering as well.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> compare "apple" "banana"
LT
ghci> compare "apple" "ap"
GT
```

You can compare tuples `(a, b)`, under the condition that `a` and `b` are both types that belong to the `Ord` class. The values from the left side of each tuple are compared first.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> compare (5, 'x') (6, 'z')
LT
```

5 is less than 6, therefore `(5, 'x')` is less than `(6, 'z')`.

If the left sides are equal, the result comes from comparing the right sides.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> compare (5, 'z') (5, 'x')
GT
```

`'z'` is greater than `'x'`, therefore `(5, 'z')` is greater than `(5, 'x')`.

#### Types not in the Ord class

`Ord` is a subclass of `Eq`, which means that any type that belongs to `Ord` must also belong to `Eq`. So the same [limitations of `Eq`](#types-for-which--is-not-defined) also apply to `Ord`: Notably, functions and `IO` actions, which are not testable for equality, are also not orderable.

In some situations, an author may be unwilling to tolerate an arbitrary ordering. One such example can be found in the [`Data.Complex`](https://hackage.haskell.org/package/base/docs/Data-Complex.html) module in the `base` package. The "complex number system" consists of number pairs (a, b) where the arithmetic meaning of the number is taken to be (a + b × √(-1)). Since complex numbers cannot be meaningfully compared in an arithmetic sense, potential for confusion has been avoided by the choice not to define an `Ord` instance at all for this type.

### Making your own type orderable

If your type belongs to the `Eq` class, it can probably belong to `Ord` as well.

#### Deriving Ord

You can get a default `Ord` instance by deriving. For the `Message` example that we gave above, we can make the type orderable by adding `Ord` to the `deriving` clause. Because we are now deriving more than one class, the `deriving` clause must now be written with the list of classes in parentheses.

```haskell
data Message = Message { subject :: Text, body :: Text }
    deriving stock (Eq, Ord)
```

```console?lang=haskell&prompt=ghci>,ghci|
ghci> compare (Message "hi" "welcome") (Message "hi" "hello")
GT
```

When you define a type with multiple constructors that can be arranged in some meaningful order, list them from least to greatest.

```haskell
data Medal = Bronze | Silver | Gold deriving stock (Eq, Ord)
```

The derived `Ord` instance respects the way the constructors are ordered in the source code.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> Gold > Silver
True
```

#### Writing Ord instances

Usually the derived `Ord` instance is good enough. But if you want your `Ord` instance to behave in some particular way, you may need to write it yourself.

##### Read the class definition

Use `:info` again, this time to inspect the `Ord` class.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> :info Ord
```

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
```

The first thing to notice is that the `Ord` class itself has a constraint: `Eq a =>` tells us that before we write an `Ord a` instance, whatever type `a` is must already have an `Eq` instance. (We sometimes say "`Ord` is a subclass of `Eq`.") So if your type does not already belong to the `Eq` class, you will need to take care of that as well.

The `Ord` class has seven methods. You can define all of these for your type, but you don't have to: The `MINIMAL` pragma tells us that we only have to define either `compare` or `(<=)`. As long as we satisfy this minimal requirement, all seven methods will work, and we'll have a complete instance of `Ord`.

##### Case-insensitive text

Consider the `CaseInsensitiveText` example from before. Since we customized `Eq`, we should also write a custom `Ord` instance.

```haskell
import Data.Text (Text)
import qualified Data.Text as Text

data CaseInsensitiveText = CaseInsensitiveText{ caseSensitiveText :: Text }

instance Eq CaseInsensitiveText
  where
    x == y =
        Text.toLower (caseSensitiveText x) ==
        Text.toLower (caseSensitiveText y)

instance Ord CaseInsensitiveText
  where
    compare x y =
        compare
          (Text.toLower (caseSensitiveText x))
          (Text.toLower (caseSensitiveText y))
```

We can now apply any `Ord` functions to `CaseInsensitiveText` values, and the results are consistent with `(==)`.

```console?lang=haskell&prompt=ghci>,ghci|
ghci> compare (CaseInsensitiveText "Hello") (CaseInsensitiveText "HELLO")
EQ
ghci> CaseInsensitiveText "Hello" == CaseInsensitiveText "HELLO"
True
ghci> CaseInsensitiveText "a" < CaseInsensitiveText "B"
True
```

##### Cleanup

There are a few things we can make more concise in the previous example.

We can simply the code above by defining `(==)` in terms of `compare`. This more concise definition spares us some effort and ensures that the two instances remain in harmony.

```haskell
instance Eq CaseInsensitiveText
  where
    x == y = compare x y == EQ
```

In the definition of `compare`, the expression `(Text.toLower (caseSensitiveText ...))` appears twice. It is common to avoid this repetition using the [`on`](https://hackage.haskell.org/package/base/docs/Data-Function.html#v:on) function.

```haskell
import Data.Function (on)

instance Ord CaseInsensitiveText
  where
    compare = compare `on` (Text.toLower . caseSensitiveText)
```

### Exercise: Concrete types

A polymorphic function can't stay polymorphic forever. When code is evaluated, every type parameter ends up taking on a concrete, monomorphic, type. For example, take the polymorphic function `(==)`:

```haskell
(==) :: Eq a -> a -> a -> Bool
```

In the very first example of this lesson (`'A' == 'A'`), the type parameter `a` assumes the concrete type `Char`. The concrete type of `(==)` in the context of that expression is:

```haskell
(==) :: Char -> Char -> Bool
```

In the expression `sort ['z', 'a', 'f']`:

* What is the result of evaluating this expression?
* What is the general (polymorphic) type of `sort`?
* What is the concrete (monomorphic) type of `sort` in this context?

In the expression `(compare 'a' 'z') /= LT`:

* What is the result of evaluating this expression?
* What is the concrete type of `compare` in this context?
* What is the concrete type of `(/=)` context?
