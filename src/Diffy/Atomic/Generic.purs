module Diffy.Atomic.Generic where

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), from)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Diffy.Atomic (class Atomic, Atom, atomize, ctor)

class GenericAtomic a where
  genericAtomize' :: a -> Atom
  genericCtor' :: a -> String

instance ctor ::
  ( GenericAtomic a
  , IsSymbol name
  ) => GenericAtomic (Constructor name a) where
    genericAtomize' (Constructor a) = genericAtomize' a
    genericCtor' _ = reflectSymbol (SProxy :: SProxy name)

instance arg ::
  ( Atomic a
  ) => GenericAtomic (Argument a) where
    genericAtomize' (Argument a) = atomize a
    genericCtor' (Argument a) = ctor a

genericAtomize :: forall a rep. Generic a rep => GenericAtomic rep => a -> Atom
genericAtomize x = genericAtomize' (from x)

genericCtor :: forall a rep. Generic a rep => GenericAtomic rep => a -> String
genericCtor x = genericCtor' (from x)