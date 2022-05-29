---
version: 1.0.0
title: Introduction to Parser Combinators
---

{% include toc.html %}

Parsers are ubiquitous in educational Haskell literature because parser combinators can be used to illustrate the power of composition.

Parsers, while being mostly associated with compilers and interpreters, can be found everywhere.

Their goal is to take input data (usually text or a stream), and produce structured output (usually a data structure, such as an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)). See [this article](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) for day to day application.

Parser Combinators are a way to build parsers by composition.

Let's say we want to extract all the tuples from some text. We can start by defining our tuple parser:

```haskell
import Text.ParserCombinators.ReadP

tuple :: ReadP (String, String)
tuple =
  between (char '(') (char ')') $ do
    left <- munch1 (/= ',')
    char ','
    skipSpaces
    right <- munch1 (/= ')')
    return (left, right)
```

As you can see, relying on `Monad` allows us to focus on the structure of the input, instead of the structure of the parser.

we can then define our main parser through composition:

```haskell
tuples :: ReadP [(String, String)]
tuples = many tuple
```

This is the power of parser combinators: since `ReadP` implements `Alternative`, any parser can be reused and extended (here via [`many`](https://hackage.haskell.org/package/base/docs/Control-Applicative.html#v:many)).

We can have a toy example:

```haskell
example :: [([(String, String)], String)]
example = readP_to_S tuples "(abc,def)(gh,   ij)(k,l)"
```

which gives us:

```haskell
[
    ([], "(abc,def)(gh,   ij)(k,l)")
,   ([("abc", "def") ], "(gh,   ij)(k,l)")
,   ([("abc", "def"), ("gh", "ij")], "(k,l)")
,   ([( "abc", "def"), ("gh", "ij"), ("k", "l")] , "")
]
```

As you can see, running `ReadP` gives all possible parsed results, which can lead to performances issues.

Going further: [Jake Wheat's Intro to Parsing with Parsec in Haskell](http://jakewheat.github.io/intro_to_parsing/)

## Targeted audience

The aim of this topic is to explore Haskell's idioms through the study of different designs.

A minimal understanding of Haskell and it's main abstractions (eg. Functor, Applicative, Monad) is expected and can be gathered in the previous courses.
