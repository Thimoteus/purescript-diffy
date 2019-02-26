module Test.Writer where

import Prelude

import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.List (List)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (under)
import Diffy (class Diffy, diffy)
import Diffy.Atomic (class Atomic, Atomizable, atomize)
import Diffy.Atomic as Atomic
import Diffy.Callback as Callback
import Diffy.Index (class Indexed, Index, Indexable, index)
import Diffy.Index as Index
import Diffy.Ordered (class Ordered, Orderable, size)
import Diffy.Ordered as Ordered
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Prim.RowList (kind RowList)

data Diff
  = Atomizable (List Index) Atomizable Atomizable
  | Orderable (List Index) Orderable Orderable
  | Indexable (List Index) Indexable Indexable

writerCb :: Callback.Callback (WriterT (Array Diff) Effect)
writerCb = Callback.mk onAtom onSize onIndex
  where

    onAtom :: forall a. Atomic a => List Index -> a -> a -> WriterT (Array Diff) Effect Unit
    onAtom path a1 a2 =
      when (atomize a1 /= atomize a2) do
        tell [Atomizable path (Atomic.mk a1) (Atomic.mk a2)]

    onSize :: forall s. Ordered s => List Index -> s -> s -> WriterT (Array Diff) Effect Unit
    onSize path l1 l2 =
      when (size l1 /= size l2) do
        tell [Orderable path (Ordered.mk l1) (Ordered.mk l2)]

    onIndex :: forall i i'. Indexed i => Indexed i' => List Index -> i -> i' -> WriterT (Array Diff) Effect Unit
    onIndex path c1 c2 =
      when (index c1 /= index c2) do
        tell [Indexable path (Index.mk c1) (Index.mk c2)]

runWriter :: forall s. Diffy s => s -> s -> Effect Unit
runWriter x y = do
  arr <- execWriterT (diffy writerCb x y)
  for_ arr case _ of
    Atomizable path a1 a2 -> do
      log $
        "Atoms different: " <> show (atomize a1) <> " vs. " <> show (atomize a2)
      log "In path:"
      traverse_ logShow path

    Orderable path l1 l2 -> do
      log $
        "Sizes different: " <> show (size l1) <> " vs. " <> show (size l2)
      log "In path:"
      traverse_ logShow path

    Indexable path c1 c2 -> do
      log $
        "Indices different: " <> show (index c1) <> " vs. " <> show (index c2)
      log "In path:"
      traverse_ logShow path

main :: Effect Unit
main = do
  log "There should be no errors:"
  runWriter 
    [ { a: unit
      , b: ""
      , c: 0
      }
    , { a: unit
      , b: "nonempty"
      , c: 10
      }
    ]

    [ { a: mempty
      , b: mempty
      , c: (under :: _ -> (Additive Int -> Additive Int) -> _)
            Additive
            (const mempty)
            100
      }
    , { a: unit * unit + unit <> unit
      , b: "non" <> "empty"
      , c: 100-90
      }
    ]

  log "There should be an atomic error:"
  runWriter
    { a: [1, 2, 3]
    , b: [4, 5, 6]
    }
    { a: [1, 2, 4]
    , b: [4, 5, 6]
    }

  log "There should be a length error:"
  runWriter
    [unit, unit, unit]
    [unit, unit]

  log "There should be an index error:"
  runWriter
    [ Left 0
    , Right 1
    , Left 2
    , Right 3
    ]
    [ Left 0
    , Right 1
    , Left 2
    , Left 3
    ]