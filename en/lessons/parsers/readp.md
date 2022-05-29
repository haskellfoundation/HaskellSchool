---
version: 1.0.0
title: ReadP
---

{% include toc.html %}

`base` embeds a parser combinators library ([`ReadP`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-ParserCombinators-ReadP.html))
as seen in the introduction.

# Definition

Right from the introduction:

> It parses all alternatives in parallel, so it never keeps hold of the beginning of the input string, a common source of space leaks with other parsers.

Let's proceed with `ReadP` definition:

```haskell
newtype ReadP a = R (forall b . (a -> P b) -> P b)

instance Functor ReadP where
  fmap h (R f) = R (\k -> f (k . h))

instance Applicative ReadP where
    pure x = R (\k -> k x)
    (<*>) = ap

instance Monad ReadP where
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

instance Alternative ReadP where
  empty = R (\_ -> Fail)
  R f1 <|> R f2 = R (\k -> f1 k <|> f2 k)
```

That's minimalist, let see how `P` is defined:

```haskell
data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final (NonEmpty (a,String))
```

It looks like `P` is the current parser's instruction.

Now, we can have a look at the associated instances:

```haskell
instance Applicative P where
  pure x = Result x Fail
  (<*>) = ap

instance Monad P where
  (Get f)         >>= k = Get (\c -> f c >>= k)
  (Look f)        >>= k = Look (\s -> f s >>= k)
  Fail            >>= _ = Fail
  (Result x p)    >>= k = k x <|> (p >>= k)
  (Final (r:|rs)) >>= k = final [ys' | (x,s) <- (r:rs), ys' <- run (k x) s]

instance Alternative P where
  empty = Fail

  -- most common case: two gets are combined
  Get f1     <|> Get f2     = Get (\c -> f1 c <|> f2 c)

  -- results are delivered as soon as possible
  Result x p <|> q          = Result x (p <|> q)
  p          <|> Result x q = Result x (p <|> q)

  -- fail disappears
  Fail       <|> p          = p
  p          <|> Fail       = p

  -- two finals are combined
  -- final + look becomes one look and one final (=optimization)
  -- final + sthg else becomes one look and one final
  Final r       <|> Final t = Final (r <> t)
  Final (r:|rs) <|> Look f  = Look (\s -> Final (r:|(rs ++ run (f s) s)))
  Final (r:|rs) <|> p       = Look (\s -> Final (r:|(rs ++ run p s)))
  Look f        <|> Final r = Look (\s -> Final (case run (f s) s of
                                []     -> r
                                (x:xs) -> (x:|xs) <> r))
  p             <|> Final r = Look (\s -> Final (case run p s of
                                []     -> r
                                (x:xs) -> (x:|xs) <> r))

  -- two looks are combined (=optimization)
  -- look + sthg else floats upwards
  Look f     <|> Look g     = Look (\s -> f s <|> g s)
  Look f     <|> p          = Look (\s -> f s <|> p)
  p          <|> Look f     = Look (\s -> p <|> f s)
```

All the logic is here, it makes more sense.

# Running the parser

There's no direct to get a result from a `ReadP`, instead we have:

```haskell
type ReadS a = String -> [(a, String)]

readP_to_S :: ReadP a -> ReadS a
readP_to_S (R f) = run (f return)

run :: P a -> ReadS a
run (Get f)         (c:s) = run (f c) s
run (Look f)        s     = run (f s) s
run (Result x p)    s     = (x,s) : run p s
run (Final (r:|rs)) _     = (r:rs)
run _               _     = []
```

Simple, looks like a basic expression interpreter, and it explains why we
might get multiple results.

We also see that error handling is implicit, you only know that parsing failed,
because there's still characters to consume.

# Conclusion

`ReadP` is a really simple parser combinators library, it's a trade off which
costs error handling and performance.
