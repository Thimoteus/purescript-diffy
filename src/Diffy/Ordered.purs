module Diffy.Ordered where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.List (List)
import Data.List as List
import Data.List.Lazy as Lazy
import Data.List.Lazy.Types as NELL
import Data.List.Types (NonEmptyList)
import Data.List.Types as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Data.RowList (RLProxy(..))

newtype Cardinality = Cardinality Int

derive newtype instance eq :: Eq Cardinality

derive newtype instance show :: Show Cardinality

class Ordered a where
  size :: a -> Cardinality

instance cardinality :: Ordered Cardinality where
  size = identity

instance int :: Ordered Int where
  size = Cardinality

instance string :: Ordered String where
  size = Cardinality <<< String.length

instance list :: Ordered (List a) where
  size = Cardinality <<< List.length

instance lazyList :: Ordered (Lazy.List a) where
  size = Cardinality <<< Lazy.length

instance nonEmptyList :: Ordered (NonEmptyList a) where
  size = size <<< NonEmptyList.toList

instance nonEmptyLazyList :: Ordered (NELL.NonEmptyList a) where
  size = size <<< NELL.toList

instance array :: Ordered (Array a) where
  size = Cardinality <<< Array.length

instance nonEmptyArray :: Ordered (NonEmptyArray a) where
  size = size <<< NonEmptyArray.toArray

instance object :: Ordered (Object a) where
  size = Cardinality <<< Object.size

instance map :: Ordered (Map k v) where
  size = Cardinality <<< Map.size

instance record ::
  ( RowList.RowToList row list
  , OrderedRec list row
  ) => Ordered (Record row) where
  size = sizeRec (RLProxy :: RLProxy list)

class OrderedRec (list :: RowList) (row :: # Type) | list -> row where
  sizeRec :: forall proxy. proxy list -> Record row -> Cardinality

instance nil :: OrderedRec RowList.Nil () where
  sizeRec _ _ = Cardinality 0

else instance cons ::
  ( OrderedRec listtail rowtail
  , IsSymbol key
  , Row.Lacks key rowtail
  , Row.Cons key el rowtail row
  ) => OrderedRec (RowList.Cons key el listtail) row where
    sizeRec _ rec =
      let
        key = SProxy :: SProxy key
        tailRec = Record.delete key rec
        listProxy = RLProxy :: RLProxy listtail
        Cardinality tailSize = sizeRec listProxy tailRec
      in
        Cardinality (tailSize + 1)

newtype Orderable = Orderable (forall r. (forall s. Ordered s => s -> r) -> r)

mk :: forall s. Ordered s => s -> Orderable
mk s = Orderable (_ $ s)

instance orderable :: Ordered Orderable where
  size (Orderable run) = run size