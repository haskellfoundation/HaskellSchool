---
version: 1.0.0
title: Parsec
---

{% include toc.html %}

[Parsec](https://hackage.haskell.org/package/parsec) is one of most famous haskell library, it embeds the concept of parser combinators.

# Definition

The main type if defined as follows:

```haskell
newtype ParsecT s u m a
    = ParsecT {unParser :: forall b .
                 State s u
              -> (a -> State s u -> ParseError -> m b) -- consumed ok
              -> (ParseError -> m b)                   -- consumed err
              -> (a -> State s u -> ParseError -> m b) -- empty ok
              -> (ParseError -> m b)                   -- empty err
              -> m b
             }
```

Whith the frontend type:

```haskell
type Parsec s u = ParsecT s u Identity
```

And the following support types:

```haskell
data Consumed a  = Consumed a
                 | Empty !a
    deriving ( Typeable )

data Reply s u a = Ok a !(State s u) ParseError
                 | Error ParseError
    deriving ( Typeable )

data State s u = State {
      stateInput :: s,
      statePos   :: !SourcePos,
      stateUser  :: !u
    }
    deriving ( Typeable )

data ParseError = ParseError !SourcePos [Message]
    deriving ( Typeable )
```

It also have a a bunch of `instance`s:

 * `MonadState`
 * `MonadReader`
 * `MonadError`
 * `MonadTrans`
 * `Monad`
 * `Functor`
 * `MonadFail`
 * `Applicative`
 * `MonadIO`
 * `Alternative`
 * `MonadPlus`
 * `MonadCont`
 * `Semigroup`
 * `Monoid`

# Construction

That's a lot, let's take a combinator to figure out each types' usage:

```haskell
between :: Stream s m t => ParsecT s u m open -> ParsecT s u m close -> ParsecT s u m a -> ParsecT s u m a
```

We start to see better:

 * `s` is the `Stream` type (eg. `Text`, `ByteString`), the input
 * `u` is the user's state
 * `m` is the underlying `Monad` (eg. `Identity`, `IO`, etc.)
 * `a` is the produced value (`ParsecT` is a `Monad`)

To go a bit deeper with the user's state, we can have a look at the related functions:

```haskell
getState :: Monad m => ParsecT s u m u
putState :: Monad m => u -> ParsecT s u m ()
modifyState :: Monad m => (u -> u) -> ParsecT s u m ()
```

# First example

If we want to play with it, we can have a parser working only on increasing numbers sequences:

```haskell
import Control.Monad
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

tokenParser :: GenTokenParser String u Identity
tokenParser = makeTokenParser emptyDef

numParser :: Parsec String Integer Integer
numParser = do
  n <- integer tokenParser
  previous <- getState
  guard $ n > previous
  putState n
  return n

increasingNumbersParser :: Parsec String Integer [Integer]
increasingNumbersParser =
  numParser `sepBy` spaces

increasingNumbers :: String -> Either ParseError [Integer]
increasingNumbers = runParser increasingNumbersParser (-1) ""
```

We have the following elements:

 * `tokenParser` which gives us a way to parse `Integer`s in `numParser`
 * `numParser`, it parses an `Integer`, check the parsed number against the state and update the state
 * `increasingNumberParser` which define the way numbers are chained
 * `increasingNumbers` which hides `Parsec` details

If we play a bit with it, here's what we get:

```
> increasingNumbers "2 9 12"
Right
    [ 2
    , 9
    , 12
    ]
> increasingNumbers "2 1 12"
Left ""
    ( line 1
    , column 5
    ) :
  unexpected "1"
```

# Key operators
In order to have a better understanding of the whole function, we have to dig
into the basic functions, to see how it's build and used.

We can start by `satisfy` which, given a predicate over a `Char`, accept it or not:

```haskell
satisfy :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
satisfy f           = tokenPrim (\c -> show [c])
                                (\pos c _cs -> updatePosChar pos c)
                                (\c -> if f c then Just c else Nothing)
```

Ok, it seems that we have to provide three functions to provide:

 * One for debugging
 * One to update position
 * One to actually do the parsing

Since it's quite different of `ParsecT`, we should had a look at `tokenPrim`:

```haskell
-- | The parser @tokenPrim showTok nextPos testTok@ accepts a token @t@
-- with result @x@ when the function @testTok t@ returns @'Just' x@. The
-- token can be shown using @showTok t@. The position of the /next/
-- token should be returned when @nextPos@ is called with the current
-- source position @pos@, the current token @t@ and the rest of the
-- tokens @toks@, @nextPos pos t toks@.
--
-- This is the most primitive combinator for accepting tokens. For
-- example, the 'Text.Parsec.Char.char' parser could be implemented as:
--
-- >  char c
-- >    = tokenPrim showChar nextPos testChar
-- >    where
-- >      showChar x        = "'" ++ x ++ "'"
-- >      testChar x        = if x == c then Just x else Nothing
-- >      nextPos pos x xs  = updatePosChar pos x

tokenPrim :: (Stream s m t)
          => (t -> String)                      -- ^ Token pretty-printing function.
          -> (SourcePos -> t -> s -> SourcePos) -- ^ Next position calculating function.
          -> (t -> Maybe a)                     -- ^ Matching function for the token to parse.
          -> ParsecT s u m a
tokenPrim showToken nextpos test
  = ParsecT $ \(State input pos user) cok _cerr _eok eerr -> do
      r <- uncons input
      case r of
        Nothing -> eerr $ unexpectError "" pos
        Just (c,cs)
         -> case test c of
              Just x -> let newpos = nextpos pos c cs
                            newstate = State cs newpos user
                        in seq newpos $ seq newstate $
                           cok x newstate (newErrorUnknown newpos)
              Nothing -> eerr $ unexpectError (showToken c) pos
```

So far so good our deductions were correct, let see what's going on:

 * We consume the first element of `State`'s `input`
 * We return an empty error if there's no element
 * We test the predicate
 * If it works:
   * We compute the new position
   * We create the new state (`input`'s remainings, new position, old user state)
   * We strictly evaluate the new position and state, then we pass it all to the successful function
 * If it fails, we use the empty error

It's clearer, now, we can see which functions are given when the parser is run:

```haskell

runParserT :: (Stream s m t)
           => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runParserT p u name s
    = do res <- runParsecT p (State s (initialPos name) u)
         r <- parserReply res
         case r of
           Ok x _ _  -> return (Right x)
           Error err -> return (Left err)
    where
        parserReply res
            = case res of
                Consumed r -> r
                Empty    r -> r
```

Nothing fancy here:

 * We run the parser with an initial state (`initialPos` starts at line 1 an column 1)
 * Transforme the reply (`Consumed`) in `Either`

We have to dig into `runParsecT` to see what's going on:

```haskell
runParsecT :: Monad m => ParsecT s u m a -> State s u -> m (Consumed (m (Reply s u a)))
runParsecT p s = unParser p s cok cerr eok eerr
    where cok a s' err = return . Consumed . return $ Ok a s' err
          cerr err = return . Consumed . return $ Error err
          eok a s' err = return . Empty . return $ Ok a s' err
          eerr err = return . Empty . return $ Error err
```

No surprise neither, just some constructor calling.

# Combinators

Since parser combinators are made from composition, we can see the `Applicative`/`Alternative`/`Monad` instances:

```haskell
instance Applicative.Applicative (ParsecT s u m) where
    pure = parserReturn
    (<*>) = ap

instance Applicative.Alternative (ParsecT s u m) where
    empty = mzero
    (<|>) = mplus

instance Monad (ParsecT s u m) where
    return = Applicative.pure
    p >>= f = parserBind p f
```

We'll have a look at `MonadPlus` later, here are `parserReturn` and `parserBind`:

```haskell
parserReturn :: a -> ParsecT s u m a
parserReturn x
    = ParsecT $ \s _ _ eok _ ->
      eok x s (unknownError s)

parserBind :: ParsecT s u m a -> (a -> ParsecT s u m b) -> ParsecT s u m b
{-# INLINE parserBind #-}
parserBind m k
  = ParsecT $ \s cok cerr eok eerr ->
    let
        -- consumed-okay case for m
        mcok x s err
          | errorIsUnknown err = unParser (k x) s cok cerr cok cerr
          | otherwise =
            let
                 -- if (k x) consumes, those go straight up
                 pcok = cok
                 pcerr = cerr

                 -- if (k x) doesn't consume input, but is okay,
                 -- we still return in the consumed continuation
                 peok x s err' = cok x s (mergeError err err')

                 -- if (k x) doesn't consume input, but errors,
                 -- we return the error in the 'consumed-error'
                 -- continuation
                 peerr err' = cerr (mergeError err err')
            in  unParser (k x) s pcok pcerr peok peerr

        -- empty-ok case for m
        meok x s err
          | errorIsUnknown err = unParser (k x) s cok cerr eok eerr
          | otherwise =
            let
                -- in these cases, (k x) can return as empty
                pcok = cok
                peok x s err' = eok x s (mergeError err err')
                pcerr = cerr
                peerr err' = eerr (mergeError err err')
            in  unParser (k x) s pcok pcerr peok peerr
        -- consumed-error case for m
        mcerr = cerr

        -- empty-error case for m
        meerr = eerr

    in unParser m s mcok mcerr meok meerr
```

`parserReturn` is fairly straight to the point: pass the value to the successful function.

While `parserBind` is way more "worked".

We can focus on `MonadPlus`:

```haskell

instance MonadPlus (ParsecT s u m) where
    -- | @mzero@ always fails without consuming any input.
    mzero
        = ParsecT $ \s _ _ _ eerr ->
          eerr $ unknownError s

    mplus m n
        = ParsecT $ \s cok cerr eok eerr ->
          let
              meerr err =
                  let
                      neok y s' err' = eok y s' (mergeError err err')
                      neerr err' = eerr $ mergeError err err'
                  in unParser n s cok cerr neok neerr
          in unParser m s cok cerr eok meerr
```

Despite the appearant complexity of `plus`, the idea is dead simple:

 * Override first parser's empty error function, such as is calls the second one
 * The second one see its errors merged with the first one

Note that they both operate on the `State`.
