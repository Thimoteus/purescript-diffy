module Diffy.Callback
  ( Callback
  , onAtom
  , onSize
  , onIndex
  , path
  , mk
  , addPath
  ) where

import Prelude

import Data.List (List(Nil), (:))
import Diffy.Atomic (class Atomic)
import Diffy.Index (class Indexed, Index)
import Diffy.Ordered (class Ordered)

data Callback m = Callback
  (forall a. Atomic a => List Index -> a -> a -> m Unit)
  (forall s. Ordered s => List Index -> s -> s -> m Unit)
  (forall i i'. Indexed i => Indexed i' => List Index -> i -> i' -> m Unit)
  (List Index)

onAtom :: forall m a. Atomic a => Callback m -> List Index -> a -> a -> m Unit
onAtom (Callback onA _ _ _) = onA

onSize :: forall s m. Ordered s => Callback m -> List Index -> s -> s -> m Unit
onSize (Callback _ onS _ _) = onS

onIndex :: forall i i' m. Indexed i => Indexed i' => Callback m -> List Index -> i -> i' -> m Unit
onIndex (Callback _ _ onI _) = onI

path :: forall m. Callback m -> List Index
path (Callback _ _ _ p) = p

mk
  :: forall m
   . Applicative m
  => (forall a. Atomic a => List Index -> a -> a -> m Unit)
  -> (forall s. Ordered s => List Index -> s -> s -> m Unit)
  -> (forall i i'. Indexed i => Indexed i' => List Index -> i -> i' -> m Unit)
  -> Callback m
mk a b c = Callback a b c Nil

addPath :: Index -> (forall m. Callback m -> Callback m)
addPath i cb = Callback (onAtom cb) (onSize cb) (onIndex cb) (i : path cb)