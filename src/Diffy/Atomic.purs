module Diffy.Atomic where

import Prelude

class (Eq a, Show a) <= Atomic a where
  atomize :: a -> Atom
  ctor :: a -> String

data Atom
  = Int Int
  | Number Number
  | Char Char
  | String String
  | Unit

derive instance eqAtom :: Eq Atom

instance showAtom :: Show Atom where
  show = case _ of
    Int n -> "(Int " <> show n <> ")"
    Number n -> "(Number " <> show n <> ")"
    Char c -> "(Char " <> show c <> ")"
    String s -> "(String " <> show s <> ")"
    Unit -> "(Unit)"

instance int :: Atomic Int where
  atomize = Int
  ctor _ = "Prim.Int"

instance number :: Atomic Number where
  atomize = Number
  ctor _ = "Prim.Number"

instance unit :: Atomic Unit where
  atomize _ = Unit
  ctor _ = "Data.Unit"

instance char :: Atomic Char where
  atomize = Char
  ctor _ = "Prim.Char"

instance string :: Atomic String where
  atomize = String
  ctor _ = "Prim.String"

instance void :: Atomic Void where
  atomize = absurd
  ctor = absurd

instance atomicAtom :: Atomic Atom where
  atomize = identity
  ctor = case _ of
    Int _ -> "Diffy.Atomic.Int"
    Number _ -> "Diffy.Atomic.Number"
    Char _ -> "Diffy.Atomic.Char"
    String _ -> "Diffy.Atomic.String"
    Unit -> "Diffy.Atomic.Unit"

newtype Atomizable
  = Atomizable (forall r. (forall a. Atomic a => a -> r) -> r)

instance eqAtomizable :: Eq Atomizable where
  eq (Atomizable run) (Atomizable run') =
    run \a ->
      run' \b ->
        ctor a == ctor b &&
        atomize a == atomize b

instance showAtomizable :: Show Atomizable where
  show (Atomizable run) = run show

instance atomizable :: Atomic Atomizable where
  atomize (Atomizable run) = run atomize
  ctor (Atomizable run) = run ctor

mk :: forall a. Atomic a => a -> Atomizable
mk x = Atomizable (_ $ x)