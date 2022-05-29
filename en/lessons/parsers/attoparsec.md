---
version: 1.0.0
title: Attoparsec
---

{% include toc.html %}

[`attoparsec`](https://hackage.haskell.org/package/attoparsec) is a well known
parser combinators library, especially for its performances.

# Definition

Having a look at its definition:

```haskell
newtype Parser i a = Parser {
      runParser :: forall r.
                   State i -> Pos -> More
                -> Failure i (State i)   r
                -> Success i (State i) a r
                -> IResult i r
    }

type family State i
type instance State ByteString = B.Buffer
type instance State Text = T.Buffer

type Failure i t   r = t -> Pos -> More -> [String] -> String
                       -> IResult i r
type Success i t a r = t -> Pos -> More -> a -> IResult i r

-- | Have we read all available input?
data More = Complete | Incomplete

newtype Pos = Pos { fromPos :: Int }
            deriving (Eq, Ord, Show, Num)

-- | The result of a parse.  This is parameterised over the type @i@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult i r =
    Fail i [String] String
    -- ^ The parse failed.  The @i@ parameter is the input that had
    -- not yet been consumed when the failure occurred.  The
    -- @[@'String'@]@ is a list of contexts in which the error
    -- occurred.  The 'String' is the message describing the error, if
    -- any.
  | Partial (i -> IResult i r)
    -- ^ Supply this continuation with more input so that the parser
    -- can resume.  To indicate that no more input is available, pass
    -- an empty string to the continuation.
    --
    -- __Note__: if you get a 'Partial' result, do not call its
    -- continuation more than once.
  | Done i r
    -- ^ The parse succeeded.  The @i@ parameter is the input that had
    -- not yet been consumed (if any) when the parse succeeded.
```

It looks like `Megaparsec`, but with a far simpler (and more specialized) `State`.

# Running the Parser

Let see how to run a `Parser` (for `ByteString`):

```haskell
parse :: Parser a -> ByteString -> Result a
parse m s = T.runParser m (buffer s) (Pos 0) Incomplete failK successK

failK :: Failure a
failK t (Pos pos) _more stack msg = Fail (Buf.unsafeDrop pos t) stack msg

successK :: Success a a
successK t (Pos pos) _more a = Done (Buf.unsafeDrop pos t) a

buffer :: ByteString -> Buffer
```

`T.runParser` is the `Parser`'s value.

Very simple, actually, error handling is limited to the position (`Pos`) and
the current processing state, are given directly.

# Combinators

Let's have a look at some combinators, starting by the usual instances:

```haskell
instance Applicative (Parser i) where
    pure v = Parser $ \t !pos more _lose succ -> succ t pos more v

instance Alternative (Parser i) where
    f <|> g = Parser $ \t pos more lose succ ->
      let lose' t' _pos' more' _ctx _msg = runParser g t' pos more' lose succ
      in runParser f t pos more lose' succ

instance Monad (Parser i) where
    m >>= k = Parser $ \t !pos more lose succ ->
        let succ' t' !pos' more' a = runParser (k a) t' pos' more' lose succ
        in runParser m t pos more lose succ'
```

Straightforward, maybe some combinators would be more interesting:

```haskell
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
  h <- peekWord8'
  if p h
    then advance 1 >> return h
    else fail "satisfy"

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
peekWord8' :: Parser Word8
peekWord8' = T.Parser $ \t pos more lose succ ->
    if lengthAtLeast pos 1 t
    then succ t pos more (Buf.unsafeIndex t (fromPos pos))
    else let succ' t' pos' more' bs' = succ t' pos' more' $! B.unsafeHead bs'
         in ensureSuspended 1 t pos more lose succ'

advance :: Int -> Parser ()
advance n = T.Parser $ \t pos more _lose succ ->
  succ t (pos + Pos n) more ()
```

Here comes the interesting part: we check the head, if it works, we increment
the position, and left the input untouched.

We can easily unterstand the reason behind `attoparsec`'s speed: a basic error
handling, and a small state.

# Zepto

`attoparsec` comes with another parser combinators type: [`Zepto`](https://hackage.haskell.org/package/attoparsec-0.14.3/docs/Data-Attoparsec-Zepto.html):

> A tiny, highly specialized combinator parser for `ByteString` strings.
> 
> While the main attoparsec module generally performs well, this module is particularly fast for simple non-recursive loops that should not normally result in failed parses.

Let's have a look:

```haskell
-- | A simple parser.
--
-- This monad is strict in its state, and the monadic bind operator
-- ('>>=') evaluates each result to weak head normal form before
-- passing it along.
newtype ZeptoT m a = Parser {
      runParser :: S -> m (Result a)
    }

type Parser a = ZeptoT Identity a

newtype S = S {
      input :: ByteString
    }

data Result a = Fail String
              | OK !a S
```

Definitively the simplest parser combinator you can come up with.

```haskell
instance (Monad m) => Applicative (ZeptoT m) where
    pure a = Parser $ \s -> return (OK a s)

instance Monad m => Alternative (ZeptoT m) where
    empty = fail "empty"

    a <|> b = Parser $ \s -> do
      result <- runParser a s
      case result of
        ok@(OK _ _) -> return ok
        _           -> runParser b s

instance Monad m => Monad (ZeptoT m) where
    m >>= k   = Parser $ \s -> do
      result <- runParser m s
      case result of
        OK a s'  -> runParser (k a) s'
        Fail err -> return (Fail err)
```

It looks tedious because you deal with `Result`, but, in the end, you have all
the boilerplate needed to create a function stored in `Parser`.

```haskell
parseT :: Monad m => ZeptoT m a -> ByteString -> m (Either String a)
parseT p bs = do
  result <- runParser p (S bs)
  case result of
    OK a _   -> return (Right a)
    Fail err -> return (Left err)
```

It comes with the following primitives:

```haskell
gets :: Monad m => (S -> a) -> ZeptoT m a
gets f = Parser $ \s -> return (OK (f s) s)

put :: Monad m => S -> ZeptoT m ()
put s = Parser $ \_ -> return (OK () s)
```

With that, are defined the only available functions: `takeWhile`, `take`, `string`, `atEnd`.

# Conclusion

`attoparsec` clearly focuses on performances, saying that, it brings two important points:

* It reaches performances by having the simplest design possible, sticking to its internals (mostly `ByteString`)
* It comes with a second abstraction, which push these principles to the extreme

Interestingly, we could assume that, in order to acheive good performances,
the code would be more cryptic, while it's the simplest implementation we have
seen so far.
