module Diffy.Generic where

import Prelude

import Data.Foldable (sequence_)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, NoConstructors, Product(..), Sum(..), from)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (SProxy(..))
import Diffy (class Diffy, diff)
import Diffy.Atomic (Atom(..))
import Diffy.Callback (DiffCb)
import Prim.RowList (kind RowList)

class GenericDiffy s where
  genericDiff' :: ∀ m. Applicative m => DiffCb m -> s -> s -> m Unit

instance genericDiffyNoCtors :: GenericDiffy NoConstructors where
  genericDiff' _ _ _ = pure unit

instance genericDiffyNoArgs :: GenericDiffy NoArguments where
  genericDiff' cb _ _ = cb.onAtom Unit Unit

instance genericDiffySum ::
  ( GenericDiffy l
  , GenericDiffy r
  , IsSymbol name
  , IsSymbol name'
  ) => GenericDiffy (Sum (Constructor name l) (Constructor name' r)) where
    genericDiff' cb l r
      = case l, r of
        Inl (Constructor x), Inl (Constructor y)
          -> genericDiff' cb x y
        Inr (Constructor x), Inr (Constructor y)
          -> genericDiff' cb x y
        _, _ ->
          let
            name = SProxy :: SProxy name
            name' = SProxy :: SProxy name'
          in
            cb.onConstructor (reflectSymbol name) (reflectSymbol name')

else instance genericDiffySum' ::
  ( GenericDiffy l
  , GenericDiffy r
  ) => GenericDiffy (Sum l r) where
    genericDiff' cb l r =
      case l, r of
        Inl x, Inl y -> genericDiff' cb x y
        Inr x, Inr y -> genericDiff' cb x y
        _, _ -> pure unit

instance genericDiffyProd ::
  ( GenericDiffy a
  , GenericDiffy b
  ) => GenericDiffy (Product a b) where
    genericDiff' cb (Product x y) (Product x' y') =
      sequence_
        [ genericDiff' cb x x'
        , genericDiff' cb y y'
        ]

instance genericDiffyCtor ::
  ( GenericDiffy a
  ) => GenericDiffy (Constructor name a) where
    genericDiff' cb (Constructor x) (Constructor y) =
      genericDiff' cb x y

instance genericDiffyArg ::
  ( Diffy a
  ) => GenericDiffy (Argument a) where
    genericDiff' cb (Argument x) (Argument y) =
      diff cb x y

genericDiff :: ∀ m s rep. Generic s rep => GenericDiffy rep => Applicative m => DiffCb m -> s -> s -> m Unit
genericDiff cb x y = genericDiff' cb (from x) (from y)