--- version: 1.4.1 title: Modules ---

We know from experience it's unruly to have all of our functions in the same
file and scope. In this lesson we're going to cover how to group entities in
modules, and group modules in components. At their most basic, modules are just
a namespace that allow us to aggregate common pieces of functionality and avoid
naming collisions. However, modules are use to build up more complex constructs
called components. Modules also offer a tool for encapsulation, by allowing us
to choose what we export.

{% include toc.html %}

## Entities, Modules, and Components

In order to understand modules we have to understand the elements that compose
a module, and what modules can be composed into. The elements that can be
exported from a module are called entities. In Haskell those are:

- Types and type synonyms
- Values: constants and functions
- Typeclasses

Modules are really just collections of these entities that are exported.
Modules can be combined through a graph of imports and exports, this is
sometimes called a dependency graph. These dependency graphs can be packaged
together as a component. The two most common components are libraries and
executables. A library allows you to export your modules for re-use in other
codebases, and an executable consists of an entrypoint (a Main module) to the
dependency graph that can be run as a program. A good example of a library is
`base` and `prelude` these libraries ship with GHC, `lens` is a good example of
a third party library that requires a package manager to import. A good example
of executables are `xmonad` and `pandoc`. Haskell's most popular package
managers also have executables that allow them to be run from the command line;
`cabal-install` and `stack`.

To reiterate, entities are our basic language-level building blocks. Modules
are groupings of entities with some or all exported. Components are graphs of
modules that can be made available in other codebases or executed via an
entrypoint module.

## Module Syntax


## Import Syntax
