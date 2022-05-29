---
version: 1.0.0
title: Appar
---

{% include toc.html %}

[Appar](https://hackage.haskell.org/package/appar) is different from the other
libraries such that, they are monadic, while `appar` is applicative.
See [this](https://stackoverflow.com/a/7863380/1599054) to have a good explanation
between the two.

# Definition

Let's start with the definition:

```haskell
data MkParser inp a = P {
    runParser :: inp -> (Maybe a, inp)
  }
```

It really looks like `attoparsec`'s `ZeptoT`, except that the `Monad` is inside
the result.

Let's see the instances:

```haskell
instance Functor (MkParser inp) where
    f `fmap` p = return f <*> p

instance Applicative (MkParser inp) where
    pure a = P $ \bs -> (Just a, bs)
    (<*>)  = ap

instance Alternative (MkParser inp) where
    empty = mzero
    (<|>) = mplus

instance Monad (MkParser inp) where
    return   = pure
    p >>= f  = P $ \bs -> case runParser p bs of
        (Nothing, bs') -> (Nothing, bs')
        (Just a,  bs') -> runParser (f a) bs'

instance MonadPlus (MkParser inp) where
    mzero       = P $ \bs -> (Nothing, bs)
    p `mplus` q = P $ \bs -> case runParser p bs of
        (Nothing, bs') -> runParser q bs'
        (Just a,  bs') -> (Just a, bs')
```

One thing surprising is the `Monad` instance for an applicative parser, we can
conclude that the '`Applicative`ness' of the library cames from the lack of
shortcut, more than the implemented solution.

# Construction

How are parsers built:

```haskell
class Eq inp => Input inp where
    -- | The head function for input
    car :: inp -> Char
    -- | The tail function for input
    cdr :: inp -> inp
    -- | The end of input
    nil :: inp
    -- | The function to check the end of input
    isNil :: inp -> Bool

satisfy :: Input inp => (Char -> Bool) -> MkParser inp Char
satisfy predicate = P sat
  where
    sat bs
      | isNil bs    = (Nothing, nil)
      | predicate b = (Just b,  bs')
      | otherwise   = (Nothing, bs)
      where
        b = car bs
        bs' = cdr bs
```

A bit abstract (and a bit lisp-like), but without surprises.

Anothor interesting one:

```haskell
try :: MkParser inp a -> MkParser inp a
try p = P $ \bs -> case runParser p bs of
        (Nothing, _  ) -> (Nothing, bs)
        (Just a,  bs') -> (Just a,  bs')
```

As expected: the parser is ran, if it succeeds, the input is consumer, or the
original input is returned.

# Running the parser

As we can expect, running the pparser will only consist in getting the computed
result:

```haskell
parse :: Input inp => MkParser inp a -> inp -> Maybe a
parse p bs = fst (runParser p bs)
```

# Conclusion

`appar` is really simple, but it is unique in the sense that it prevent
context-dependant parsing.

