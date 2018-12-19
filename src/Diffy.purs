module Diffy where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sequence_, traverse_)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), uncurry)
import Data.Variant (SProxy(..))
import Diffy.Atomic (class Atomic, Atom(..), Atomizable(..), atomize)
import Diffy.Callback (DiffCb)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Data.RowList (RLProxy(..))

class Diffy s where
  diff :: ∀ m. Applicative m => DiffCb m -> s -> s -> m Unit

instance maybe ::
  ( Diffy a
  ) => Diffy (Maybe a) where
    diff cb ma ma' = case ma, ma' of
      Just a, Just a' -> diff cb a a'
      Nothing, Just _ -> cb.onConstructor "Nothing" "Just"
      Just _, Nothing -> cb.onConstructor "Just" "Nothing"
      _, _ -> cb.onAtom Unit Unit

instance either ::
  ( Diffy l
  , Diffy r
  ) => Diffy (Either l r) where
    diff cb e1 e2 = case e1, e2 of
      Left l, Left l' -> diff cb l l'
      Left l, Right r -> cb.onConstructor "Left" "Right"
      Right r, Right r' -> diff cb r r'
      Right r, Left l -> cb.onConstructor "Right" "Left"

instance tuple ::
  ( Diffy a
  , Diffy b
  ) => Diffy (Tuple a b) where
    diff cb (Tuple x y) (Tuple x' y') =
      sequence_
        [ diff cb x x'
        , diff cb y y'
        ]

instance array ::
  ( Diffy el
  ) => Diffy (Array el) where
    diff cb a1 a2
      | l1 <- Array.length a1
      , l2 <- Array.length a2
      , l1 /= l2
      = sequence_
          [ traverse_ (uncurry (diff cb)) (Array.zip a1 a2)
          , cb.onLength l1 l2
          ]
      | otherwise
      = traverse_ (uncurry (diff cb)) (Array.zip a1 a2)

instance list ::
  ( Diffy el
  ) => Diffy (List el) where
    diff cb = go 0 where
      go len lst1 lst2 =
        case lst1, lst2 of
          x : xs, y : ys ->
            sequence_
              [ diff cb x y
              , go (len + 1) xs ys
              ]
          Nil, Nil -> cb.onAtom Unit Unit
          xs, ys ->
            cb.onLength (List.length xs + len) (List.length ys + len)

instance lazyList ::
  ( Diffy el
  ) => Diffy (LL.List el) where
    diff cb = go 0 where
      go len lst1 lst2 =
        case LL.uncons lst1, LL.uncons lst2 of
          Just xs, Just ys ->
            sequence_
              [ diff cb xs.head ys.head
              , go (len + 1) xs.tail ys.tail
              ]
          Nothing, Nothing -> cb.onAtom Unit Unit
          Just xs, _ ->
            cb.onLength (LL.length xs.tail + 1 + len) len
          _, Just ys ->
            cb.onLength len (LL.length ys.tail + 1 + len)

instance record ::
  ( RowList.RowToList row rl
  , DiffyRec rl row
  ) => Diffy (Record row) where
    diff = diffRec (RLProxy :: RLProxy rl)

instance number :: Diffy Number where
  diff = diffAtomic

instance int :: Diffy Int where
  diff = diffAtomic

instance unit :: Diffy Unit where
  diff = diffAtomic

instance char :: Diffy Char where
  diff = diffAtomic

instance string :: Diffy String where
  diff = diffAtomic

instance void :: Diffy Void where
  diff = diffAtomic

instance atomizable ::
  Diffy Atomizable where
    diff cb (Atomizable runA) (Atomizable runB) =
      runA \a -> runB \b -> cb.onAtom (atomize a) (atomize b)

class DiffyRec (rl :: RowList) (row :: # Type) | rl -> row where
  diffRec
    :: ∀ m proxy
     . Applicative m
    => proxy rl
    -> DiffCb m
    -> Record row
    -> Record row
    -> m Unit

instance diffNil :: DiffyRec RowList.Nil () where
  diffRec _ _ _ _ = pure unit

else instance diffCons ::
  ( DiffyRec rltail rowtail
  , IsSymbol key
  , Row.Lacks key rowtail
  , Row.Cons key el rowtail row
  , Diffy el
  ) => DiffyRec (RowList.Cons key el rltail) row where
    diffRec _ cb r1 r2
      | key <- SProxy :: SProxy key
      , r1tail <- Record.delete key r1
      , r2tail <- Record.delete key r2
      , r1el <- Record.get key r1
      , r2el <- Record.get key r2
      = sequence_
          [ diff cb r1el r2el
          , diffRec (RLProxy :: RLProxy rltail) cb r1tail r2tail
          ]

diffNewtype :: ∀ m n t. Newtype n t => Diffy t => Applicative m => DiffCb m -> n -> n -> m Unit
diffNewtype cb n1 n2 = diff cb (unwrap n1) (unwrap n2)

diffAtomic :: ∀ m a. Atomic a => Applicative m => DiffCb m -> a -> a -> m Unit
diffAtomic cb a1 a2 = cb.onAtom (atomize a1) (atomize a2)
