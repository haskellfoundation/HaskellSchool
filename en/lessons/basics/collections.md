---
version: 1.3.1
title: Collections
---

Tuples, Lists, Assoc Lists, Sets, Maps and Hashmaps.

{% include toc.html %}

## Tuples

Tuples are the first kind of collection you are introduced to in haskell. This is because they are simple, primitive, and have a terse, flexible, built in syntax. Tuples are sometimes referred to as anonymous records, but instead of referencing fields by name you reference them by position in the structure. Tuples can contain an arbitrary number of items, but we will focus on the 2-tuple since haskell has good built in support for them. However, if you are interested this is what an 8-tuple looks like

```haskell
Prelude> :t ('0','1','2','3','4','5','6',"8-tuple")
('0','1','2','3','4','5','6',"8-tuple")
  :: (Char, Char, Char, Char, Char, Char, Char, [Char])
```
__Note__: `:t` in the ghci repl shows the type of the value, it will print out a message of the form `term :: Type`

### Constructing Tuples

We can construct tuples using a familiar syntax `(a,b)`.

```haskell
Prelude> myTuple = (1 :: Int,"hello")
Prelude> :t myTuple
myTuple :: (Int, [Char])
```

### Consuming Tuples

We can consume tuples by breaking them apart. With a 2-tuple the options are pretty simple. They have the predictable names `fst` and `snd`

```haskell
Prelude> fst myTuple
1
Prelude> snd myTuple
"hello"
```

We can also pattern match on tuples to extract their contents.

```haskell
Prelude> (\(a,b) -> a + 2) $ myTuple
3
Prelude> (\(a,b) -> b ++ " world") $ myTuple
"string world"
```

### Limitations of Tuples

There are a couple of limitations with tuples in haskell. First, they are closed; you cannot add an additional item once the tuple of length n has been defined. Second, the types are fixed; once a tuple has a concrete type you cannot add an element of a different type. You can get around this with specialized functions, but they do not generalize to all tuples lengths.

Here is an example of increasing the tuple's length.

```haskell
Prelude> twoTupleToThreeTuple c (a,b) = (a,b,c)
Prelude> twoTupleToThreeTuple () myTuple
(1,"world",())
```

Here is an example of changing a tuple's type.

```haskell
Prelude> :t myTuple
myTuple :: (Int, [Char])
Prelude> :t substituteFst "hello" myTuple
substituteFst "hello" myTuple :: ([Char], [Char])
Prelude> substituteFst "hello" myTuple
("hello","world")
```
## Lists

From a usability perspective, Lists solve the problem of extensibility that tuples face, but they can only contain one type (in other words they are homogenous). Lists also have a special built in syntax.

```haskell
Prelude> [1,2,3,4]
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
Prelude> [1,2,3,4]
[1,2,3,4]
Prelude> 1 : 2 : 3 : 4 : []
[1,2,3,4]
Prelude> 1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` Nil
1 `Cons` (2 `Cons` (3 `Cons` (4 `Cons` Nil)))
```
__Note__: If you want to get the `List` example working in ghci you will have to input some special commands to make it work ``data List a = Nil | a `Cons` List a deriving Show; infixr 5 `Cons`;``.


### List Concatenation

List concatenation uses the `++` operator:

```haskell
Prelude> [1, 2] ++ [3, 4, 1]
[1, 2, 3, 4, 1]
```

A side note about another operator that you might come across `<>`, this is an example of typeclass abstraction in haskell, it allows us to associate specific implementations of functions (and operators) with specific types. In the case of lists `<>` is exactly `++` under the hood. The full typeclass implemetation looks like this:

```haskell
instance Semigroup [a] where
  (<>) = (++)
```

The keywords aren't important, but it should give you an intuition for when you see `<>` which just means concatenation for a specific type!

### List Subtraction

Support for subtraction is provided via the `--/2` operator; it's safe to subtract a missing value:

```elixir
iex> ["foo", :bar, 42] -- [42, "bar"]
["foo", :bar]
```

Be mindful of duplicate values.
For every element on the right, the first occurrence of it gets removed from the left:

```elixir
iex> [1,2,2,3,2,3] -- [1,2,3,2]
[2, 3]
```

**Note:** List subtraction uses [strict comparison](../basics/#comparison) to match the values. For example:

```elixir
iex> [2] -- [2.0]
[2]
iex> [2.0] -- [2.0]
[]
```

### Head / Tail

When using lists, it is common to work with a list's head and tail.
The head is the list's first element, while the tail is a list containing the remaining elements.
Elixir provides two helpful functions, `hd` and `tl`, for working with these parts:

```elixir
iex> hd [3.14, :pie, "Apple"]
3.14
iex> tl [3.14, :pie, "Apple"]
[:pie, "Apple"]
```

In addition to the aforementioned functions, you can use [pattern matching](../pattern-matching/) and the cons operator `|` to split a list into head and tail. We'll learn more about this pattern in later lessons:

```elixir
iex> [head | tail] = [3.14, :pie, "Apple"]
[3.14, :pie, "Apple"]
iex> head
3.14
iex> tail
[:pie, "Apple"]
```

## Tuples

Tuples are similar to lists, but are stored contiguously in memory.
This makes accessing their length fast but modification expensive; the new tuple must be copied entirely to memory.
Tuples are defined with curly braces:

```elixir
iex> {3.14, :pie, "Apple"}
{3.14, :pie, "Apple"}
```

It is common for tuples to be used as a mechanism to return additional information from functions; the usefulness of this will be more apparent when we get into [pattern matching](../pattern-matching/):

```elixir
iex> File.read("path/to/existing/file")
{:ok, "... contents ..."}
iex> File.read("path/to/unknown/file")
{:error, :enoent}
```

## Keyword lists

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
