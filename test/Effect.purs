module Test.Effect where


import Prelude

import Data.Foldable (traverse_)
import Data.List (List)
import Diffy (diffy)
import Diffy.Atomic (class Atomic)
import Diffy.Callback as Callback
import Diffy.Index (class Indexed, Index, index)
import Diffy.Ordered (class Ordered, size)
import Effect (Effect)
import Effect.Console (log, logShow)
import Prim.RowList (kind RowList)

effectCb :: Callback.Callback Effect
effectCb = Callback.mk onAtom onSize onIndex where

    onAtom :: forall a. Atomic a => List Index -> a -> a -> Effect Unit
    onAtom path a1 a2 = when (a1 /= a2) do
      log "Atoms different:"
      logShow a1
      logShow a2

      log "In path:"
      traverse_ logShow path


    onSize :: forall s. Ordered s => List Index -> s -> s -> Effect Unit
    onSize path s1 s2 = do
      log "Sizes different:"
      logShow (size s1)
      logShow (size s2)

      log "In path:"
      traverse_ logShow path

    onIndex :: forall i i'. Indexed i => Indexed i' => List Index -> i -> i' -> Effect Unit
    onIndex path i1 i2 = do
      log "Indices different:"
      logShow (index i1)
      logShow (index i2)

      log "In path:"
      traverse_ logShow path

main :: Effect Unit
main = do
  diffy effectCb unit unit
