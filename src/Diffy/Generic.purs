module Diffy.Generic where

import Prelude

import Data.Foldable (sequence_)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, NoConstructors, Product(..), Sum(..), from)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (SProxy(..))
import Diffy (class Diffy, diffy, onAtom, onIndex)
import Diffy.Atomic (Atom(..))
import Diffy.Callback (Callback, addPath, path)
import Diffy.Index as Index

class GenericDiffy s where
  genericDiffy' :: forall m. Applicative m => Callback m -> s -> s -> m Unit

instance noCtors :: GenericDiffy NoConstructors where
  genericDiffy' _ _ _ = pure unit

instance noArgs :: GenericDiffy NoArguments where
  genericDiffy' cb _ _ = onAtom cb (path cb) Unit Unit

instance sum ::
  ( GenericDiffy l
  , GenericDiffy r
  , IsSymbol name
  , IsSymbol name'
  ) => GenericDiffy (Sum (Constructor name l) (Constructor name' r)) where
    genericDiffy' cb l r
      = case l, r of
        Inl (Constructor x), Inl (Constructor y)
          -> genericDiffy'
              (addPath (Index.index (Index.TCProxy :: Index.TCProxy name)) cb) x y
        Inr (Constructor x), Inr (Constructor y)
          -> genericDiffy'
              (addPath (Index.index (Index.TCProxy :: Index.TCProxy name')) cb) x y
        _, _ ->
          let
            name = SProxy :: SProxy name
            name' = SProxy :: SProxy name'
          in
            onIndex cb (path cb) (reflectSymbol name) (reflectSymbol name')

else instance sum' ::
  ( GenericDiffy l
  , GenericDiffy r
  ) => GenericDiffy (Sum l r) where
    genericDiffy' cb l r =
      case l, r of
        Inl x, Inl y -> genericDiffy' cb x y
        Inr x, Inr y -> genericDiffy' cb x y
        _, _ -> pure unit

instance prod ::
  ( GenericDiffy a
  , GenericDiffy b
  ) => GenericDiffy (Product a b) where
    genericDiffy' cb (Product x y) (Product x' y') =
      sequence_
        [ genericDiffy' cb x x'
        , genericDiffy' cb y y'
        ]

instance ctor ::
  ( GenericDiffy a
  , IsSymbol name
  ) => GenericDiffy (Constructor name a) where
    genericDiffy' cb (Constructor x) (Constructor y) =
      genericDiffy' (addPath (Index.index (Index.TCProxy :: Index.TCProxy name)) cb) x y

instance arg ::
  ( Diffy a
  ) => GenericDiffy (Argument a) where
    genericDiffy' cb (Argument x) (Argument y) =
      diffy cb x y

genericDiffy :: forall m s rep. Generic s rep => GenericDiffy rep => Applicative m => Callback m -> s -> s -> m Unit
genericDiffy cb x y = genericDiffy' cb (from x) (from y)