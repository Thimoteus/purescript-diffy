module Diffy
  ( class Diffy
  , diffy
  , atomicDiff
  , module Exports
  , class DiffyRec
  , diffyRec
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.List.Lazy as LL
import Data.List.Lazy.Types as NELL
import Data.List.Types (NonEmptyList)
import Data.List.Types as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..))
import Diffy.Atomic (class Atomic, Atom(..), Atomizable)
import Diffy.Callback (Callback, addPath, onAtom, onIndex, onSize, path, mk) as Exports
import Diffy.Callback (Callback, addPath, onAtom, onIndex, onSize, path)
import Diffy.Index (DCProxy(..), Index(..), TCProxy(..), index)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Data.RowList (RLProxy(..))

class Diffy s where
  diffy :: forall m. Applicative m => Callback m -> s -> s -> m Unit

instance maybe ::
  ( Diffy a
  ) => Diffy (Maybe a) where
    diffy cb ma ma' = case ma, ma' of
      Just a, Just a' -> diffy (addDataConstructor _just cb) a a'
      Nothing, Just _ -> onIndex cb (path cb) _nothing _just
      Just _, Nothing -> onIndex cb (path cb) _just _nothing
      _, _ -> onAtom cb (path cb) Unit Unit

instance either ::
  ( Diffy a
  , Diffy b
  ) => Diffy (Either a b) where
    diffy cb e1 e2 = case e1, e2 of
      Left l, Left l' -> diffy (addDataConstructor _left cb) l l'
      Right r, Right r' -> diffy (addDataConstructor _right cb) r r'
      Left _, Right _ -> onIndex cb (path cb) _left _right
      Right _, Left _ -> onIndex cb (path cb) _right _left

instance tuple ::
  ( Diffy a
  , Diffy b
  ) => Diffy (Tuple a b) where
    diffy cb (Tuple x y) (Tuple x' y') =
      sequence_
        [ diffy (addLabel _fst cb) x x'
        , diffy (addLabel _snd cb) y y'
        ]

instance array ::
  ( Diffy el
  ) => Diffy (Array el) where
    diffy cb a1 a2
      = sequence_
          [
            let
              a = Array.zip a1 a2
              f i (Tuple e1 e2) = diffy (addTypeConstructor _array $ addPlace i cb) e1 e2
            in
              traverseWithIndex_ f a
          , let
              l1 = Array.length a1
              l2 = Array.length a2
            in
              unless (l1 == l2) $ onSize cb (path cb) l1 l2
          ]

instance nonEmptyArray ::
  ( Diffy el
  ) => Diffy (NonEmptyArray el) where
    diffy cb a1 a2 =
      diffy
        (addTypeConstructor _nonEmptyArray cb)
        (NonEmptyArray.toArray a1)
        (NonEmptyArray.toArray a2)

instance list ::
  ( Diffy el
  ) => Diffy (List el) where
    diffy cb = go 0 where
      go len lst1 lst2 =
        case lst1, lst2 of
          x : xs, y : ys ->
            sequence_
              [ diffy (addTypeConstructor _list $ addPlace len cb) x y
              , go (len + 1) xs ys
              ]
          Nil, Nil -> onAtom cb (path cb) Unit Unit
          xs, ys ->
            onSize cb (path cb) (List.length xs + len) (List.length ys + len)

instance nonEmptyList ::
  ( Diffy el
  ) => Diffy (NonEmptyList el) where
    diffy cb lst1 lst2 =
      diffy
        (addTypeConstructor _nonEmptyList cb)
        (NonEmptyList.toList lst1)
        (NonEmptyList.toList lst2)

instance lazyList ::
  ( Diffy el
  ) => Diffy (LL.List el) where
    diffy cb = go 0 where
      go len lst1 lst2 =
        case LL.uncons lst1, LL.uncons lst2 of
          Just xs, Just ys ->
            sequence_
              [ diffy (addTypeConstructor _lazyList cb) xs.head ys.head
              , go (len + 1) xs.tail ys.tail
              ]
          Nothing, Nothing -> onAtom cb (path cb) Unit Unit
          Just xs, _ ->
            onSize cb (path cb) (LL.length xs.tail + 1 + len) len
          _, Just ys ->
            onSize cb (path cb) len (LL.length ys.tail + 1 + len)

instance nonEmptyLazyList ::
  ( Diffy el
  ) => Diffy (NELL.NonEmptyList el) where
    diffy cb lst1 lst2 =
      diffy (addTypeConstructor _nonEmptyLazyList cb) (NELL.toList lst1) (NELL.toList lst2)

instance map ::
  ( Diffy key
  , Diffy val
  ) => Diffy (Map key val) where
    diffy cb map1 map2 = go (Map.toUnfoldable map1) (Map.toUnfoldable map2) 0
      where
      go (Tuple key val : xs) (Tuple key' val' : ys) k
        = sequence_
          [ diffy (addTypeConstructor _map cb) key key'
          , diffy (addTypeConstructor _map cb) val val'
          , go xs ys (k + 1)
          ]
      go Nil (_ : ys) k =
        onSize cb (path cb) k (List.length ys + k + 1)
      go (_ : xs) Nil k =
        onSize cb (path cb) (List.length xs + k + 1) k
      go _ _ _ = onAtom cb (path cb) Unit Unit

instance set ::
  ( Diffy el
  , Ord el
  ) => Diffy (Set el) where
    diffy cb = go 0 where
      go len set1 set2
        | Just (Tuple el1 sub1) <- popMin set1
        , Just (Tuple el2 sub2) <- popMin set2
        = sequence_
            [ diffy (addTypeConstructor _set cb) el1 el2
            , go (len + 1) sub1 sub2
            ]
        | otherwise
        = onSize cb (path cb) (Set.size set1 + len) (Set.size set2 + len)

instance nonEmptySet ::
  ( Diffy el
  , Ord el
  ) => Diffy (NonEmptySet el) where
    diffy cb set1 set2 =
      diffy
        (addTypeConstructor _nonEmptySet cb)
        (NonEmptySet.toSet set1)
        (NonEmptySet.toSet set2)

instance object ::
  ( Diffy el
  ) => Diffy (Object el) where
    diffy cb obj1 obj2 =
      go (Object.toAscUnfoldable obj1) (Object.toAscUnfoldable obj2) 0
      where
      go (Tuple key val : xs) (Tuple key' val' : ys) k
        = sequence_
            [ if key /= key'
              then onIndex (addTypeConstructor _object cb) (path cb) key key'
              else diffy (addTypeConstructor _object cb) val val'
            , go xs ys (k + 1)
            ]
      go Nil (_ : ys) k = onSize cb (path cb) k (k + List.length ys + 1)
      go (_ : xs) Nil k = onSize cb (path cb) (1 + List.length xs + k) k
      go _ _ _ = onAtom cb (path cb) Unit Unit

instance record ::
  ( RowList.RowToList row rl
  , DiffyRec rl row
  ) => Diffy (Record row) where
    diffy = diffyRec (RLProxy :: RLProxy rl)

instance number :: Diffy Number where
  diffy = atomicDiff

instance int :: Diffy Int where
  diffy = atomicDiff

instance unit :: Diffy Unit where
  diffy _ _ _ = pure unit -- sidesteps the needless equality check

instance char :: Diffy Char where
  diffy = atomicDiff

instance string :: Diffy String where
  diffy = atomicDiff

instance void :: Diffy Void where
  diffy _ = absurd

instance atomizable ::
  Diffy Atomizable where
    diffy = atomicDiff

class DiffyRec (rl :: RowList) (row :: # Type) | rl -> row where
  diffyRec
    :: forall m proxy
     . Applicative m
    => proxy rl
    -> Callback m
    -> Record row
    -> Record row
    -> m Unit

instance diffRecNil :: DiffyRec RowList.Nil () where
  diffyRec _ cb _ _ = onAtom cb (path cb) Unit Unit

else instance diffRecCons ::
  ( DiffyRec rltail rowtail
  , IsSymbol key
  , Row.Lacks key rowtail
  , Row.Cons key el rowtail row
  , Diffy el
  ) => DiffyRec (RowList.Cons key el rltail) row where
    diffyRec _ cb r1 r2
      | key <- SProxy :: SProxy key
      , r1tail <- Record.delete key r1
      , r2tail <- Record.delete key r2
      , r1el <- Record.get key r1
      , r2el <- Record.get key r2
      = sequence_
          [ diffy (addLabel key $ addTypeConstructor _record cb) r1el r2el
          , diffyRec (RLProxy :: RLProxy rltail) cb r1tail r2tail
          ]

atomicDiff :: forall m a. Atomic a => Applicative m => Callback m -> a -> a -> m Unit
atomicDiff cb a1 a2
  | a1 /= a2 = onAtom cb (path cb) a1 a2
  | otherwise = pure unit

popMin :: forall a. Ord a => Set a -> Maybe (Tuple a (Set a))
popMin x = ado
  el <- Set.findMin x
  in Tuple el (Set.delete el x)

addLabel :: forall sym. IsSymbol sym => SProxy sym -> (forall m. Callback m -> Callback m)
addLabel = addPath <<< Label <<< reflectSymbol

addDataConstructor :: forall sym. IsSymbol sym => DCProxy sym -> (forall m. Callback m -> Callback m)
addDataConstructor = addPath <<< index

addTypeConstructor :: forall sym. IsSymbol sym => TCProxy sym -> (forall m. Callback m -> Callback m)
addTypeConstructor = addPath <<< index

addPlace :: Int -> (forall m. Callback m -> Callback m)
addPlace = addPath <<< Place

_just :: DCProxy "Just"
_just = DCProxy

_nothing :: DCProxy "Nothing"
_nothing = DCProxy

_left :: DCProxy "Left"
_left = DCProxy

_right :: DCProxy "Right"
_right = DCProxy

_fst :: SProxy "fst"
_fst = SProxy

_snd :: SProxy "snd"
_snd = SProxy

_array :: TCProxy "Array"
_array = TCProxy

_nonEmptyArray :: TCProxy "NonEmptyArray"
_nonEmptyArray = TCProxy

_list :: TCProxy "List"
_list = TCProxy

_nonEmptyList :: TCProxy "NonEmptyList"
_nonEmptyList = TCProxy

_lazyList :: TCProxy "LazyList"
_lazyList = TCProxy

_nonEmptyLazyList :: TCProxy "NonEmptyLazyList"
_nonEmptyLazyList = TCProxy

_map :: TCProxy "Map"
_map = TCProxy

_set :: TCProxy "Set"
_set = TCProxy

_nonEmptySet :: TCProxy "NonEmptySet"
_nonEmptySet = TCProxy

_object :: TCProxy "Object"
_object = TCProxy

_variant :: TCProxy "Variant"
_variant = TCProxy

_record :: TCProxy "Record"
_record = TCProxy
