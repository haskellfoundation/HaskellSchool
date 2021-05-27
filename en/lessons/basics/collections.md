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
data List a = Nil | a `Cons` List a
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
ghci> 1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` Nil
1 `Cons` (2 `Cons` (3 `Cons` (4 `Cons` Nil)))
```
__Note__: If you want to get the `List` example working in ghci you will have to
input some special commands to make it work ``data List a = Nil | a `Cons` List
a deriving Show; infixr 5 `Cons`;``.

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
