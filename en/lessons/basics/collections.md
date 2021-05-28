---
version: 1.3.1
title: Collections
---

Tuples, Lists, Assoc Lists, Sets, Maps/Hashmaps, Seqs, and Vectors.

{% include toc.html %}

## Tuples

Tuples are the first kind of collection you are introduced to in Haskell. This
is because they are simple, primitive, and have a terse, flexible, built in
syntax. Tuples are sometimes referred to as anonymous records, but instead of
referencing fields by name you reference them by position in the structure.
Tuples can contain an arbitrary number of items, well this is not entirely
true, the Haskell Report only requires up to 15-tuples and GHC comes with
support for 62-tuples, but usually that is enough.We will focus on the 2-tuple
since Haskell has good built in support for them. However, if you are
interested this is what an 8-tuple looks like

```haskell
ghci> :t ('0', '1', '2', '3', '4', '5', '6', "8-tuple")
('0', '1', '2', '3', '4', '5', '6', "8-tuple")
  :: (Char, Char, Char, Char, Char, Char, Char, [Char])
```
__Note__: `:t` in the ghci repl shows the type of the value, it will print out a
message of the form `term :: Type`

### When to Use

Tuples are not very flexible because they have a fixed length. This also means
we have a lot of information about the structure of our data and therefore
access is very perfomant `O(1)`. From a type safety perspective tuples are
useful for associating data; saying "I required both of these things". For
example if we wanted to represent a money transfer it wouldn't make sense to
have just the amount or just the destination, we would want to group them `(Int,
AccountId)`.

There is an interesting parallel between functions that require n arguments and
functions that require n-tuples. While "curried" functions are arguably more
useful due to partial application, these two modalities are equivalent.

```haskell
ghci> :t (&&)
(&&) :: Bool -> Bool -> Bool
ghci> :t uncurry (&&)
uncurry (&&) :: (Bool, Bool) -> Bool
ghci> :t curry (uncurry (&&))
curry (uncurry (&&)) :: Bool -> Bool -> Bool
```

### Constructing Tuples

We can construct tuples using a familiar syntax `(a,b)`.

```haskell
ghci> myTuple = (True,"hello")
ghci> :t myTuple
myTuple :: (Bool, [Char])
```

### Consuming Tuples

We can consume tuples by breaking them apart. With a 2-tuple the options are
pretty simple. They have the predictable names `fst` and `snd`, and are
available in the standard library, so no need to import anything!

```haskell
ghci> fst myTuple
True
ghci> snd myTuple
"hello"
```

The types of these functions simple, yet reveal exactly how they behave. `fst`
picks the first element and `snd` picks the second element.

```haskell
ghci> :t fst
fst :: (a, b) -> a
ghci> :t snd
snd :: (a, b) -> b
```

We can also pattern match on tuples to extract their contents.

```haskell
ghci> (\(a,b) -> not a) myTuple
False
ghci> (\(a,b) -> b ++ " world") myTuple
"hello world"
```

### Limitations of Tuples

The main limitation with tuples in Haskell is that each different length of
tuple is a distinct data type. That means there is no general function to add an
additional element to any tuple. Rather these kinds of functions need to be
defined ad hoc.

Here is an example of increasing the tuple's length.

```haskell
ghci> twoTupleToThreeTuple c (a,b) = (a,b,c)
ghci> twoTupleToThreeTuple () myTuple
(1,"world",())
```

Trying to call our function on a tuple of the incorrect length results in a type
error telling us just as much; the function expected a 2-tuple but we gave it a
3-tuple.

```haskell
ghci> twoTupleToThreeTuple True (1,2,3)

<interactive>:19:27: error:
    • Couldn't match expected type: (a, b)
                  with actual type: (a0, b0, c0)
                  ...
```
## Lists

From a usability perspective, Lists solve the problem of extensibility that
tuples face, but they can only contain one type (in other words they are
homogenous). Lists also have a special built in syntax.

```haskell
ghci> [1,2,3,4]
[1,2,3,4]
ghci> ["hello", True]

<interactive>:22:11: error:
    • Couldn't match type ‘Bool’ with ‘[Char]’
      Expected: String
        Actual: Bool
    ...
```

### Inductive Types

Lists are also a first introduction to an "inductive" or "recursive" type. Here
is an example that is identical to the Haskell implementation without the
syntactic sugar (we use backticks to add infix syntax).

```haskell
data List a = Nil | Cons a (List a)
```

You can see that this type is recursive. `Nil` is the base case, and the `Cons`
constructor joins an `a` and a recursive call to `List a`. You can also see why
lists can only contain 1 type, because the `a` is threaded through the entire
structure. In our definition we can replace `Nil` with the empty list `[]` and
`Cons` with `:` to get back to the builtin syntax.

In that case our original example breaks down like so:

```haskell
ghci> [1,2,3,4]
[1,2,3,4]
ghci> 1 : 2 : 3 : 4 : []
[1,2,3,4]
ghci> Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
```

### List Performance

Haskell implements lists as linked lists. Where the cons cells (this operator is
called a cons `:`) act as the links.  This means that accessing the list length
is an operation that will run in linear time (`O(n)`).  For this reason, it is
typically faster to prepend than to append:

```Haskell
ghci> list = ["Orange", "Banana", "Apple"]
["Orange", "Banana", "Apple"]
-- Prepending (fast) (O(1))
ghci> "Grape" : list
["Grape", "Orange", "Banana", "Apple"]
-- Appending (slow) (O(n))
ghci> list ++ ["Cherry"]
["Orange", "Banana", "Apple", "Cherry"]
```

### When to Use

Linked lists are an extremely common data structure in functional programming,
this means you are going to see them everywhere in Haskell. They are generally
the first container a Haskell programmer reaches for.  Because of slow append
and relatively slow indexed access (O(n) where n is the index) they are
generally used in situations where you know you are going to have to iterate
over the entire data set, or you want to preserve the order of elements.

A great example of an abstract data structure for which a linked list is a great
concrete implementation are stacks. This is because pushing and popping are
O(1).

A bad use case would be for a queue, where either enqueue or dequeue would be
O(n) depending on which side of the linked list you decide to insert into.

A common real world example of where lists are used is database queries. A
database query could return no results `[]`, or some `[entity..]`, and there is
potentially an ordering to these results. The database library doesn't really
care about indexed access, and so it leaves that consideration to the caller of
the function.

### List Concatenation

List concatenation uses the `++` operator:

```haskell
ghci> [1, 2] ++ [3, 4, 1]
[1, 2, 3, 4, 1]
```

A side note about another operator that you might come across `<>`. This
operator is sometimes referred to as `mappend`, it represents generic
concatenation. In other words any type that supports `<>` can be "appended". In
the case of lists `<>` is exactly `++` under the hood.

### Head / Tail

When using lists, it is common to work with a list's head and tail. The head is
the list's first element, while the tail is a list containing the remaining
elements.

Haskell provides two helpful functions, `head` and `tail`, for working with
these parts:

```haskell
ghci> head ["Orange", "Banana", "Apple"]
"Orange"
ghci> tail ["Orange", "Banana", "Apple"]
["Banana","Apple"]
```

Unfortunately these functions reveal an ugly part of the language's base
library, some of the functions are partial. This means that they do not cover
the full domain of possible inputs.

```haskell
ghci> head []
*** Exception: Prelude.head: empty list
ghci> tail []
*** Exception: Prelude.tail: empty list
```

We can use a common idiom in Haskell for covering partial functions in a safe
way, the `Maybe` type. This allows us to say that unhandled inputs return a
`Nothing`. Now the caller of this maybe-returning-function must handle the
`Nothing` case, but in return they are not faced with a nasty runtime exception.

```haskell
ghci> :i Maybe
data Maybe a = Nothing | Just a 	-- Defined in ‘GHC.Maybe’
...
```

__Note__: `:i` in ghci will give you some information about the type, the first
line is the implementation.

Now we can define a total head and tail function using pattern matching!

```haskell
ghci> :{
| safeHead :: [a] -> Maybe a
| safeHead [] = Nothing
| safeHead (x:xs) = Just x
|
| safeTail :: [a] -> Maybe [a]
| safeTail [] = Nothing
| safeTail (x:xs) = Just xs
| :}
ghci> safeHead ["Orange", "Banana", "Apple"]
Just "Orange"
ghci> safeHead []
Nothing
ghci> safeTail ["Orange", "Banana", "Apple"]
Just ["Banana","Apple"]
ghci> safeTail []
Nothing
```

__Note__: `:{` and `:}` allow you to write multiline definitions in ghci.

Hooray! No more exceptions.

Another collection that is the cousin of the list, and ensures that `head` and
`tail` are safe is the nonempty list.

```haskell
ghci> import Data.List.NonEmpty
ghci> :i NonEmpty
type NonEmpty :: * -> *
data NonEmpty a = a :| [a]
  	-- Defined in ‘GHC.Base’
```

From its definition we can see that `NonEmpty` requires the first element to be
present. Despite its similarity to the sum constructor `|`, the `:|` constructor
of `NonEmpty` is just a plane old binary constructor; just a fancy looking
tuple!

This handles the partiality problem the exact opposite way as the
`Maybe` solution. Instead of forcing the caller of the function to guard against
the ill-defined case when handling the result of the function, it forces them to
construct a valid input up front (when calling the function).

```haskell
ghci> :t Data.List.NonEmpty.head
Data.List.NonEmpty.head :: NonEmpty a -> a
ghci> Data.List.NonEmpty.head (1 :| [])
1
ghci> Data.List.NonEmpty.head []

<interactive>:28:25: error:
    • Couldn't match expected type: NonEmpty a
                  with actual type: [a0]
    ...
```

Notice that this time the error is not a runtime exception but a type error, the
compiler is politely telling us that we tried to use a (potentially empty) list
rather than the required `NonEmpty` type.

## Assoc lists

So far we have only been able to access values via position indexed lookup, or
pattern matching. However, one of the most common use cases for containers is
acting as a key value store. Assoc(iation) lists provide this by combining
2-tuples and regular lists. Since assoc lists are really just the combination
of two existing data types, the only thing we need is a lookup function, which
is provided in the `Data.List` module in base.

```haskell
ghci> assoc = [("foo", True), ("bar", False)]
ghci> :t assoc
assoc :: [(String, Bool)]
ghci> :t lookup
lookup :: Eq a => a -> [(a, b)] -> Maybe b
ghci> lookup "foo" assoc
Just True
ghci> lookup "bar" assoc
Just False
ghci> lookup "baz" assoc
Nothing
```

We can see the pattern, again, where undefined behaviour is encoded using the
`Maybe` type. This allows us to have complete functions with errors at compile
time rather than runtime.

It is also interesting to note the `Eq a` constraint on the "key" which allows
the lookup function to do an equality comparison on them.

While assoc lists are a nice introduction to key value collections that build
on the previous types we learned about, they are not particularly useful. A list
simply isn't a very good data structure for lookup, as it provides worst case
`O(n)` asymptotics. Assoc lists are usually an intermediate data structure
which Haskell programmers will usually convert into a `Map`. Although this
conversion is itself an `O(n*log n)` operation `Map` provides an `O(log n)`
lookup.

## Sets

Sets are a very interesting container, the core concept of a set is membership.
Although you can lookup elements by index, this is generally not advised since
the interface makes no promises about what this index will be. Instead if is
common to build up sets and test to see if an element exists in that set.

A set can be constructed purely by inserting elements into the empty set

```haskell
ghci> import Data.Set
ghci> :t empty
empty :: Set a
ghci> insert 1 (insert 2 (insert 3 empty))
fromList [1,2,3]
```

Or by creating a set from a list

```haskell
ghci> fromList [4,3,2,1]
fromList [1,2,3,4]
```

You might notice that the elements are sorted after turning them into a `Set`. This is
becuase internally the `Set` data type depends on its contents being orderable. We can
see this from the constraints on the `insert` and `fromList` functions.

```haskell
ghci> :t insert
insert :: Ord a => a -> Set a -> Set a
ghci> :t fromList
fromList :: Ord a => [a] -> Set a
```

Sets have a very useful property, they cannot contain duplicates. This means that `insert`
is idempotent.

```haskell
ghci> insert1 = insert 1
ghci> insert1 empty
fromList [1]
ghci> insert1 (insert1 empty)
fromList [1]
ghci> insert1 (insert1 (insert1 empty))
fromList [1]
```
__Note__: Idempotence is the property that some functions have, where calling the function
multiple times has the same result. For idempotent function f `f a == f (f a)`.

This also means that calling `toList` after `fromList` is an inefficient way to
de-duplicate a list!

```haskell
ghci> toList $ fromList [1,1]
[1]
```

Alright, let's see a use case for sets.

```haskell
ghci> :t member
member :: Ord a => a -> Set a -> Bool
ghci> evens = fromList [0,2..1000000]
ghci> member 7 evens
False
ghci> member 200012 evens
True
ghci> isEven n = member n evens
ghci> isEven 7
False
ghci> isEven 8
True
```

You might say "hmmm a `1000000` limit for even numbers seems incorrect",
and you would be correct. This highlights a property of sets in haskell, they
are finite due to the strictness in the underlying implementation.

### Difference

Set difference is a good way to break apart a set based on a subset. Say I want
to get a set of all consonants, and I have a set of all characters.

```haskell
ghci> alphabet = fromList ['a'..'z']
ghci> alphabet
fromList "abcdefghijklmnopqrstuvwxyz"
ghci> vowels = fromList ['a', 'e', 'i', 'o', 'u', 'y']
ghci> vowels
fromList "aeiouy"
ghci> difference alphabet vowels
fromList "bcdfghjklmnpqrstvwxz"
```

This brings up a critical point of order! The `difference` function subtracts its second argument
from the first, so mixing up your sets can lead to undesired behaviour.

```haskell
ghci> difference vowels alphabet
fromList ""
```

You also may have noticed that `y`, which can be both a consonant and a vowel,
was subtracted. Let's fix that.

```haskell
ghci> consonants = difference alphabet $ difference vowels (singleton 'y')
ghci> consonants
fromList "bcdfghjklmnpqrstvwxyz"
```

### Union

Union's are useful for when we want to build up a new set from two smaller
sets. We can actually rephrase the "set arithmetic" we used to construct the set
of consonants in the previous example to use a `union`. If the `difference` function
is kind of like subtraction, then the `union` function is kind of like addition.

Here is the algebra in case you need convincing.

```
a - (b - c) = (a - b) + c
4 - (2 - 1) = (4 - 2) + 1
3 = 3
```

```haskell
ghci> union (difference alphabet vowels) (singleton 'y')
fromList "bcdfghjklmnpqrstvwxyz"
ghci> consonants == union (difference alphabet vowels) (singleton 'y')
True
```

There is an important difference between regular arithmetic and "set
arithmetic", because sets cannot contain duplicates `union` does not always
behave the same as `+`.

In regular arithmetic this relationship still holds.

```
a - (b - c) = (a + c) - b
4 - (2 - 1) = (4 + 1) - 2
3 = 3
```

But it doesn't work for our example.

```haskell
ghci> difference (union alphabet $ singleton 'y') vowels
fromList "bcdfghjklmnpqrstvwxz"
```

This is because `union alphabet $ singleton 'y'` is actually the same set as `alphabet`.

### Intersection

Intersection allows us to find elements that sets have in common.

```haskell
ghci> intersection vowels consonants
fromList "y"
```

### Subsets

Subsets allow us to detect if all the elements of a set are contained within another set.

```haskell
ghci> isSubsetOf vowels alphabet
True
ghci> isSubsetOf consonants alphabet
True
ghci> isSubsetOf alphabet vowels
False
ghci> isSubsetOf consonants vowels
False
```

This function, like `difference`, has an order to the arguments. We are asking
if the first argument is a subset of the second. From the example above you can
see that `vowels` are a subset of the `alphabet` but not the other way around.

### Cartesian Products

Cartesion products, sometimes referred to as cross multiplication, allows us to
get all the possible combinations of the elements of two sets.

A classic example of a cartesian product is a chess board, which is the cross
multiplication of the letters a - h and the numbers 1 - 8.

```haskell
ghci> ranks = fromList [1..8]
ghci> files = fromList ['a'..'h']
ghci> cartesianProduct files ranks
fromList
  [('a',1),('a',2),('a',3),('a',4),('a',5),('a',6),('a',7),('a',8)
  ,('b',1),('b',2),('b',3),('b',4),('b',5),('b',6),('b',7),('b',8)
  ,('c',1),('c',2),('c',3),('c',4),('c',5),('c',6),('c',7),('c',8)
  ,('d',1),('d',2),('d',3),('d',4),('d',5),('d',6),('d',7),('d',8)
  ,('e',1),('e',2),('e',3),('e',4),('e',5),('e',6),('e',7),('e',8)
  ,('f',1),('f',2),('f',3),('f',4),('f',5),('f',6),('f',7),('f',8)
  ,('g',1),('g',2),('g',3),('g',4),('g',5),('g',6),('g',7),('g',8)
  ,('h',1),('h',2),('h',3),('h',4),('h',5),('h',6),('h',7),('h',8)
  ]
```

You're can thank me when you ace the n-queens question on your next technical
interview.

## Maps

In Haskell, maps are the "go-to" key-value store, sometimes referred to as a
dictionary.

While you can construct a `Map` with any type as the key and value, almost all
functions that operate on maps require that the key has an ordering (`Ord k`).
This is because the internal implementation of the `Map` type in haskell is a
size balanced binary tree.

The `Map` data type and the functions for interacting with it are exported from
`Data.Map` which is a module in the `containers` package. Don't worry about
dependencies though, containers is a core library and ships with ghci.

The simplest way to construct a `Map` is to call `fromList` on an assoc list.

```haskell
ghci> letterToChar = [("a", 'a'), ("b", 'b'), ("c", 'c')]
ghci> fromList letterToChar
fromList [("a",'a'),("b",'b'),("c",'c')]
ghci> letterToCharMap = fromList letterToChar
ghci> :t letterToCharMap
letterToCharMap :: Map String Char
ghci> Data.Map.lookup "a" letterToCharMap
Just 'a'
```

__Note__: You might remember that there is a lookup function that works on
assoc lists `Eq a => a -> [(a,b)] -> Maybe b`. Because both `Prelude` and
`Data.Map` export functions of the same name we need to qualify our use so ghc
knows which function we mean. That is why we have to type out `Data.Map.lookup`
in the snippet above

`Map` use the same data structure internally as `Set`, and luckily `containers`
exposes very uniform api's. So we get all of our set operations on maps, and
they are just as performant! Lists also have the set operations but they are
much less performant due to the different performance characteristics of linked
lists and binary trees.

## Adding Values to a Map

The blessed function for adding to a map is called `insert`. If a duplicate key
is added to a map, it will replace the former value:

```haskell
ghci> empty
fromList []
ghci> oneItem = insert "first" 1 empty
ghci> oneItem
fromList [("first",1)]
ghci> insert "first" 2 oneItem
fromList [("first",2)]
```

A useful function to be aware of is `adjust`. This lets us update a value at a
specified key, only if it exists, if not the old map is returned.

```haskell
ghci> adjust (+2) "first" oneItem
fromList [("first",3)]
ghci> :t adjust
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
ghci> adjust (+2) "second" oneItem
fromList [("first",1)]
```

The observant reader will notice that `adjust` doesn't actually update the map.
The second invocation of adjust returns the original `oneItem` map, this makes
sense when you consider that all data in haskell is immutable!

## When to use Maps

Maps are really great for in memory persistence of state that will need to be
retrieved by a key of some arbitrary type. This is is because maps have great
lookup asymptotics (`O(log n)`) due to the ordering on the key values. The
example that immediately springs to mind is session storage on the server, in a
web application. The session state can be indexed by an id that is stored in
the cookie.

## Hashmaps

There are some cases where we do not have an ordering on our type, but still
want to use it as a key to index a map. In this case we probably want to reach
for a hashmap. A hashmap simply hashes the key, et voilà, we have an ordering!
This does require that the key type is hashable though.

The module that exports the `HashMap` data type and functionality is called
`Data.HashMap.Strict`, it lives in the `unordered-containers` package.
Unfortunately `unordered-containers` are not included in ghci (as of 9.0.1) so
you will have to setup a project in order to pull in that dependency.

## Seqs

## Vectors
