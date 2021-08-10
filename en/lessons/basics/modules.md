---
version: 1.0.0
title: Modules
---

We know from experience it's unruly to have all of our functions in the same
file and scope. In this lesson we're going to cover how to group entities in
modules, and group modules in components. At their most basic, modules are just
a namespace that allows us to aggregate common pieces of functionality and
avoid naming collisions. However, modules are used to build up more complex
constructs called components. Modules also offer a tool for encapsulation, by
allowing us to choose what we export.

{% include toc.html %}

## Entities, Modules, and Components

In order to understand modules we have to understand the elements that compose
a module, and what modules can be composed into. The elements that can be
exported from a module are called entities. In Haskell those are:

- Types and type synonyms
- Values: constants and functions
- Typeclasses

Modules are really just collections of these entities that are exported.
Modules can be combined through a graph of imports and exports. This dependency
graph can be packaged as a component. The two most common components are
libraries and executables. A library allows you to export your modules for
re-use in other codebases, and an executable consists of an entrypoint (a Main
module) to the dependency graph that can be run as a program. A good example of
a library is `base` which exports the Prelude. The `base` library ships with GHC,
whereas a third party library that requires a package manager is `lens`. Well
known haskell executables are [`xmonad`](https://xmonad.org/), [`pandoc`](https://pandoc.org/) and [`shellcheck`](https://www.shellcheck.net). Haskell's most popular
package managers also have executables that allow them to be run from the
command line: `cabal-install` and `stack`.

To reiterate, an entity is our basic language-level building block. Modules
are groupings of entities with some or all exported. Components are graphs of
modules that can be made available in other codebases or executed via an
entrypoint module.

## Module Syntax

Module syntax is driven by 2 keywords, `module` and `where`. The `module` keyword
precedes the module's name, which must match the filename, and optionally a
collection of entities to export. The `where` keyword ends the module
declaration and indicates the beginning of the module.

Below is an example of a module declaration exposing all entities.

```haskell
-- src/Example.hs
module Example where

id :: a -> a
id a = a

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \a -> f (g a)
```

Here is the same module, but residing at a different filepath.

```haskell
-- src/Data/Examples/Example.hs
module Data.Examples.Example where

id :: a -> a
id a = a

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \a -> f (g a)
```

Modules can export a subset of the entities they contain. In the example below
only the type `Identity` (and its constructor) and `id` are exported. The
`compose` function is completely isolated to the local module scope.

```haskell
-- src/Identity.hs
module Identity (
  Identity(..),
  id
) where

data Identity a = Identity a

id :: a -> a
id a = a

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \a -> f (g a)
```
__Note__: you'll learn about using and defining types in Haskell in
[types](../types) and [algebraic data types](../algebraic-data-types).

Modules can also export types, but hide constructors. This means (with respect
to the example below) that you can speak about things of type `Identity` but you
cannot construct them yourself. These are sometimes referred to as opaque types.

```haskell
-- src/Identity.hs
module Identity (
  Identity,
  wrapIdentity
) where

data Identity a = Identity a

wrapIdentity :: a -> Identity a
wrapIdentity = Identity
```

There is only one piece of syntax that ever comes before module syntax; that is
language extensions. We won't go over what they do here, but just so you are
familiar this is what they look like.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Identity (
  Identity,
  wrapIdentity
) where

data Identity a = Identity a

wrapIdentity :: a -> Identity a
wrapIdentity = Identity
```

## Import Syntax

Modules can be listed as a dependency within other modules. It is convention to
have these declarations at the top of a file, below the module declaration.

This is an "open" import, and it brings all entities exported by the module into
scope.

```haskell
module Example where

import Data.Text
```

Sometimes we want to be explicit about the entities that we are bringing into
scope. The syntax below brings in all declared entities, and hides all the
entities that are elided.

```haskell
module Example where

import Data.Text (head)
```

Another technique is to qualify an import so that all use sites are tagged.


```haskell
module Example where

import qualified Data.Text

textHead = Data.Text.head
```

Note, you can use qualifications even if a module is not imported as
`qualified`. This highlights that qualification enforces a namespace, but we can
always optionally declare the namespace. This is useful for resolving naming
collisions. In this example I will use ghci to demonstrate a name collision error.

```console?lang=haskell&prompt=ghci>,ghci|
Prelude> import Data.Map
Prelude Data.Map> map

<interactive>:2:1: error:
    Ambiguous occurrence ‘map’
    It could refer to
       either ‘Data.Map.map’,
              imported from ‘Data.Map’
              (and originally defined in ‘Data.Map.Internal’)
           or ‘Prelude.map’,
              imported from ‘Prelude’ (and originally defined in ‘GHC.Base’)
Prelude Data.Map> :t Data.Map.map
Data.Map.map :: (a -> b) -> Map k a -> Map k b
Prelude Data.Map> :t Prelude.map
Prelude.map :: (a -> b) -> [a] -> [b]
```

If `Data.Text` is too long to type out, we can provide an alias for our import
when qualifying it.

```haskell
module Example where

import qualified Data.Text as T

textHead = T.head
```
