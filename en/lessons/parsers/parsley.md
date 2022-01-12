---
version: 1.0.0
title: Parsley
---

{% include toc.html %}

From the documentation:

> [parsley](https://hackage.haskell.org/package/parsley) is a staged selective
> parser combinator library, which means it does not support monadic operations,
> and relies on Typed Template Haskell to generate very fast code.

# Definition

Let's seen how it's defined:

```haskell
newtype Parser a = Parser {unParser :: Fix (Combinator :+: ScopeRegister) a}

-- Core datatype
data Combinator (k :: Type -> Type) (a :: Type) where
  Pure           :: Defunc a -> Combinator k a
  Satisfy        :: Defunc (Char -> Bool) -> Combinator k Char
  (:<*>:)        :: k (a -> b) -> k a -> Combinator k b
  (:*>:)         :: k a -> k b -> Combinator k b
  (:<*:)         :: k a -> k b -> Combinator k a
  (:<|>:)        :: k a -> k a -> Combinator k a
  Empty          :: Combinator k a
  Try            :: k a -> Combinator k a
  LookAhead      :: k a -> Combinator k a
  Let            :: Bool -> MVar a -> Combinator k a
  NotFollowedBy  :: k a -> Combinator k ()
  Branch         :: k (Either a b) -> k (a -> c) -> k (b -> c) -> Combinator k c
  Match          :: k a -> [Defunc (a -> Bool)] -> [k b] -> k b -> Combinator k b
  Loop           :: k () -> k a -> Combinator k a
  MakeRegister   :: ΣVar a -> k a -> k b -> Combinator k b
  GetRegister    :: ΣVar a -> Combinator k a
  PutRegister    :: ΣVar a -> k a -> Combinator k ()
  Position       :: PosSelector -> Combinator k Int
  Debug          :: String -> k a -> Combinator k a
  MetaCombinator :: MetaCombinator -> k a -> Combinator k a
```

Clearly, that's the complete opposite approach of everything we have seen so
far, each operation has a constructor.

We can also have a look at their associated types:

```haskell

data ScopeRegister (k :: Type -> Type) (a :: Type) where
  ScopeRegister :: k a -> (forall r. Reg r a -> k b) -> ScopeRegister k b

data PosSelector where
  Line :: PosSelector
  Col  :: PosSelector

{-|
This is an opaque representation of a parsing register.
It is the abstracted representation of a runtime storage location.
-}
newtype Reg (r :: Type) a = Reg (ΣVar a)

data MetaCombinator where
  -- | After this combinator exits, a cut has happened
  Cut         :: MetaCombinator
  -- | This combinator requires a cut from below to respect parsec semantics
  RequiresCut :: MetaCombinator
  -- | This combinator denotes that within its scope, cut semantics are not enforced
  CutImmune   :: MetaCombinator

{-|
An identifier representing concrete registers and mutable state.
-}
newtype ΣVar (a :: Type) = ΣVar IΣVar

{-|
Underlying untyped identifier, which is numeric but otherwise opaque.
-}
newtype IΣVar = IΣVar Word64 deriving newtype (Ord, Eq, Num, Enum, Show, Ix)

{-|
This datatype is useful for providing an /inspectable/ representation of common Haskell functions.
-}
data Defunc a -- complex implementation

```

Not particularly interesting, but it is very detailed.

# Running the parser

We can have a look at how everything is used, from the example:

```haskell
parseOut :: ByteString -> Maybe [Out]
parseOut = $$(Parsley.parse myParser)
```

Then we expect the `Template` work to be here:

```haskell
parse :: (Trace, Input input) => Parser a -> Code (input -> Maybe a)
parse p = [||\input -> $$(eval [||input||] (compile (try p) codeGen))||]
```

The implementation is quite complex, but it'll hopefully help to get a better picture:

```haskell
{-|
Translates a parser represented with combinators into its machine representation.
-}
{-# INLINEABLE codeGen #-}
codeGen :: Trace
        => Maybe (MVar x)   -- ^ The name of the parser, if it exists.
        -> Fix Combinator x -- ^ The definition of the parser.
        -> Set SomeΣVar     -- ^ The free registers it requires to run.
        -> IMVar            -- ^ The binding identifier to start name generation from.
        -> LetBinding o a x

eval :: forall o a. (Trace, Ops o) => Code (InputDependant o) -> LetBinding o a a -> DMap MVar (LetBinding o a) -> Code (Maybe a)

compile :: forall compiled a. Trace => Parser a -> (forall x. Maybe (MVar x) -> Fix Combinator x -> Set IΣVar -> IMVar -> IΣVar -> compiled x) -> (compiled a, DMap MVar compiled)
```

The idea is to compile `Combinator` to `Template`'s `Code` through a complete machinery.

# Conclusion

The special feature of `parsley` is to be mostly working at compile-time,
making the implementation far more complex.
