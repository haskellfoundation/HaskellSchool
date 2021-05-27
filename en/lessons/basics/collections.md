---
version: 1.3.1
title: Collections
---

Tuples, Lists, Assoc Lists, Sets, Maps/Hashmaps, Seqs, and Vectors.

{% include toc.html %}

## Tuples

Tuples are the first kind of collection you are introduced to in haskell. This is because they are simple, primitive, and have a terse, flexible, built in syntax. Tuples are sometimes referred to as anonymous records, but instead of referencing fields by name you reference them by position in the structure. Tuples can contain an arbitrary number of items, but we will focus on the 2-tuple since haskell has good built in support for them. However, if you are interested this is what an 8-tuple looks like

```haskell
ghci> :t ('0','1','2','3','4','5','6',"8-tuple")
('0','1','2','3','4','5','6',"8-tuple")
  :: (Char, Char, Char, Char, Char, Char, Char, [Char])
```
__Note__: `:t` in the ghci repl shows the type of the value, it will print out a message of the form `term :: Type`

### Constructing Tuples

We can construct tuples using a familiar syntax `(a,b)`.

```haskell
ghci> myTuple = (1 :: Int,"hello")
ghci> :t myTuple
myTuple :: (Int, [Char])
```

### Consuming Tuples

We can consume tuples by breaking them apart. With a 2-tuple the options are pretty simple. They have the predictable names `fst` and `snd`

```haskell
ghci> fst myTuple
1
ghci> snd myTuple
"hello"
```

We can also pattern match on tuples to extract their contents.

```haskell
ghci> (\(a,b) -> a + 2) $ myTuple
3
ghci> (\(a,b) -> b ++ " world") $ myTuple
"string world"
```

### Limitations of Tuples

There are a couple of limitations with tuples in haskell. First, they are closed; you cannot add an additional item once the tuple of length n has been defined. Second, the types are fixed; once a tuple has a concrete type you cannot add an element of a different type. You can get around this with specialized functions, but they do not generalize to all tuples lengths.

Here is an example of increasing the tuple's length.

```haskell
ghci> twoTupleToThreeTuple c (a,b) = (a,b,c)
ghci> twoTupleToThreeTuple () myTuple
(1,"world",())
```

Here is an example of changing a tuple's type.

```haskell
ghci> :t myTuple
myTuple :: (Int, [Char])
ghci> :t substituteFst "hello" myTuple
substituteFst "hello" myTuple :: ([Char], [Char])
ghci> substituteFst "hello" myTuple
("hello","world")
```
## Lists

From a usability perspective, Lists solve the problem of extensibility that tuples face, but they can only contain one type (in other words they are homogenous). Lists also have a special built in syntax.

```haskell
ghci> [1,2,3,4]
[1,2,3,4]
```

Haskell implements list collections as linked lists.
This means that accessing the list length is an operation that will run in linear time (`O(n)`).
For this reason, it is typically faster to prepend than to append:

```Haskell
iex> list = ["Orange", "Banana", "Apple"]
["Orange", "Banana", "Apple"]
# Prepending (fast)
iex> "Grape" : list
["Grape", "Orange", "Banana", "Apple"]
# Appending (slow)
iex> list ++ ["Cherry"]
["Orange", "Banana", "Apple", "Cherry"]
```

Lists are also a first introduction to an "inductive" or "recursive" type. Here is an example that is identical to the haskell implementation without the syntactic sugar (we use backticks to add infix syntax).

```haskell
data List a = Nil | a `Cons` List a
```

You can see that this type is recursive. `Nil` is the base case, and the `Cons` constructor joins an `a` and a recursive call to `List a`. You can also see why lists can only contain 1 type, because the `a` is threaded through the entire structure. In our definition we can replace `Nil` with the empty list `[]` and `Cons` with `:` to get back to the builtin syntax.

In that case our original example breaks down like so:

```haskell
ghci> [1,2,3,4]
[1,2,3,4]
ghci> 1 : 2 : 3 : 4 : []
[1,2,3,4]
ghci> 1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` Nil
1 `Cons` (2 `Cons` (3 `Cons` (4 `Cons` Nil)))
```
__Note__: If you want to get the `List` example working in ghci you will have to input some special commands to make it work ``data List a = Nil | a `Cons` List a deriving Show; infixr 5 `Cons`;``.


### List Concatenation

List concatenation uses the `++` operator:

```haskell
ghci> [1, 2] ++ [3, 4, 1]
[1, 2, 3, 4, 1]
```

A side note about another operator that you might come across `<>`, this is an example of typeclass abstraction in haskell, it allows us to associate specific implementations of functions (and operators) with specific types. In the case of lists `<>` is exactly `++` under the hood. The full typeclass implemetation looks like this:

```haskell
instance Semigroup [a] where
  (<>) = (++)
```

The keywords aren't important, but it should give you an intuition for when you see `<>` which just means concatenation for a specific type!

### List Difference

Support for difference is provided via the `\\` operator; it's safe to subtract a missing value:

```haskell
ghci> import Data.List
ghci> ["foo", "bar", "baz"] \\ ["quux", "bar"]
["foo","baz"]
```

__Note__: `import` syntax may be new to you, but this allows us to pull in libraries into the repl, and fortunately for us GHC ships with a list module in its base library!

Be mindful of duplicate values. For every element on the right, the first occurrence of it gets removed from the left:

```haskell
ghci> ["foo", "bar", "baz", "bar"] \\ ["quux", "bar"]
["foo","baz","bar"]
```

List difference uses an [equality](../basics/#comparison) instance for your specific type, to match values. We can see this in the type.

```haskell
ghci> :t (\\)
(\\) :: Eq a => [a] -> [a] -> [a]
```

The `Eq a` on the left hand side of the constraint arrow `=>` means that all `a`'s on the right must provide an implementation of equality. This allows us to compare the elements in the list and subtract them.

### Head / Tail

When using lists, it is common to work with a list's head and tail.
The head is the list's first element, while the tail is a list containing the remaining elements.

Haskell provides two helpful functions, `head` and `tail`, for working with these parts:

```haskell
ghci> head ["Orange", "Banana", "Apple"]
"Orange"
ghci> tail ["Orange", "Banana", "Apple"]
["Banana","Apple"]
```

Unfortunately these functions reveal an ugly part of the language's base library, some of the functions are partial. This means that they do not cover the full domain of possible inputs.

```haskell
ghci> head ["Orange", "Banana", "Apple"]
"Orange"
ghci> tail ["Orange", "Banana", "Apple"]
["Banana","Apple"]
```

We can use a common idiom in haskell for covering partial functions in a safe way, the `Maybe` type. This allows us to say that unhandled inputs return a `Nothing`. Now the caller of this maybe-returning-function must handle the `Nothing` case, but in return they are not faced with a nasty runtime exception.

```haskell
ghci> :i Maybe
data Maybe a = Nothing | Just a 	-- Defined in ‘GHC.Maybe’
...
```

__Note__: `:i` in ghci will give you some information about the type, the first line is the implementation.

Now we can define a total head and tail function using pattern matching!

```haskell
ghci> :{
ghci Data.List| safeHead :: [a] -> Maybe a
ghci Data.List| safeHead [] = Nothing
ghci Data.List| safeHead (x:xs) = Just x
ghci Data.List|
ghci Data.List| safeTail :: [a] -> Maybe [a]
ghci Data.List| safeTail [] = Nothing
ghci Data.List| safeTail (x:xs) = Just xs
ghci Data.List| :}
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

## Assoc lists

Keyword lists and maps are the associative collections of Elixir.
In Elixir, a keyword list is a special list of two-element tuples whose first element is an atom; they share performance with lists:

```elixir
iex> [foo: "bar", hello: "world"]
[foo: "bar", hello: "world"]
iex> [{:foo, "bar"}, {:hello, "world"}]
[foo: "bar", hello: "world"]
```

The three characteristics of keyword lists highlight their importance:

+ Keys are atoms.
+ Keys are ordered.
+ Keys do not have to be unique.

For these reasons, keyword lists are most commonly used to pass options to functions.

## Maps

In Elixir, maps are the "go-to" key-value store.
Unlike keyword lists, they allow keys of any type and are un-ordered.
You can define a map with the `%{}` syntax:

```elixir
iex> map = %{:foo => "bar", "hello" => :world}
%{:foo => "bar", "hello" => :world}
iex> map[:foo]
"bar"
iex> map["hello"]
:world
```

As of Elixir 1.2, variables are allowed as map keys:

```elixir
iex> key = "hello"
"hello"
iex> %{key => "world"}
%{"hello" => "world"}
```

If a duplicate is added to a map, it will replace the former value:

```elixir
iex> %{:foo => "bar", :foo => "hello world"}
%{foo: "hello world"}
```

As we can see from the output above, there is a special syntax for maps containing only atom keys:

```elixir
iex> %{foo: "bar", hello: "world"}
%{foo: "bar", hello: "world"}
iex> %{foo: "bar", hello: "world"} == %{:foo => "bar", :hello => "world"}
true
```

In addition, there is a special syntax for accessing atom keys:

```elixir
iex> map = %{foo: "bar", hello: "world"}
%{foo: "bar", hello: "world"}
iex> map.hello
"world"
```

Another interesting property of maps is that they provide their own syntax for updates (note: this creates a new map):

```elixir
iex> map = %{foo: "bar", hello: "world"}
%{foo: "bar", hello: "world"}
iex> %{map | foo: "baz"}
%{foo: "baz", hello: "world"}
```

**Note**: this syntax only works for updating a key that already exists in the map! If the key does not exist, a `KeyError` will be raised.

To create a new key, instead use [`Map.put/3`](https://hexdocs.pm/elixir/Map.html#put/3)

```elixir
iex> map = %{hello: "world"}
%{hello: "world"}
iex> %{map | foo: "baz"}
** (KeyError) key :foo not found in: %{hello: "world"}
    (stdlib) :maps.update(:foo, "baz", %{hello: "world"})
    (stdlib) erl_eval.erl:259: anonymous fn/2 in :erl_eval.expr/5
    (stdlib) lists.erl:1263: :lists.foldl/3
iex> Map.put(map, :foo, "baz")
%{foo: "baz", hello: "world"}
```
