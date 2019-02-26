module Test.Coverage where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Diffy (class Diffy, Callback, mk, diffy) as Diffy
import Diffy.Atomic (class Atomic, Atomizable) as Diffy
import Diffy.Index (class Indexed, Index, Indexable, display, index) as Diffy
import Diffy.Ordered (class Ordered, Orderable, size) as Diffy
import Effect (Effect)
import Effect.Class.Console (log)

data Diff
  = A (List Diffy.Index) Diffy.Atomizable Diffy.Atomizable
  | O (List Diffy.Index) Diffy.Orderable  Diffy.Orderable
  | I (List Diffy.Index) Diffy.Indexable  Diffy.Indexable

callback :: Diffy.Callback Effect
callback = Diffy.mk a o i where
  a :: forall a. Diffy.Atomic a => List Diffy.Index -> a -> a -> Effect Unit
  a path x y = when (x /= y) do
    log $ show x <> " ≠ " <> show y
    traverse_ (log <<< Diffy.display) path

  o :: forall o. Diffy.Ordered o => List Diffy.Index -> o -> o -> Effect Unit
  o path x y = do
    log $ show (Diffy.size x) <> " ≠ " <> show (Diffy.size y)
    traverse_ (log <<< Diffy.display) path

  i :: forall i i'. Diffy.Indexed i => Diffy.Indexed i' => List Diffy.Index -> i -> i' -> Effect Unit
  i path x y = do
    log $ show (Diffy.index x) <> " ≠ " <> show (Diffy.index y)
    traverse_ (log <<< Diffy.display) path

run :: forall d. Diffy.Diffy d => d -> d -> Effect Unit
run = Diffy.diffy callback

maybe :: Effect Unit
maybe = do
  run Nothing (Nothing :: Maybe Int)
  run (Just 0) Nothing
  run Nothing (Just 0)
  run (Just 0) (Just 1)
  run (Just 1) (Just 0)

either :: Effect Unit
either = do
  run (Left 0) (Left 0 :: Either Int Int)
  run (Left 0) (Left 1 :: Either Int Int)
  run (Left 1) (Left 0 :: Either Int Int)
  run (Left 0) (Right 0 :: Either Int Int)
  run (Right 0) (Left 0 :: Either Int Int)
  run (Right 0) (Right 1 :: Either Int Int)
  run (Right 1) (Right 0 :: Either Int Int)
  run (Right 0) (Right 0 :: Either Int Int)

tuple :: Effect Unit
tuple = do
  run (Tuple 0 0) (Tuple 0 0)
  run (Tuple 1 0) (Tuple 0 0)
  run (Tuple 0 1) (Tuple 0 0)
  run (Tuple 0 0) (Tuple 1 0)
  run (Tuple 0 0) (Tuple 0 1)
  run (Tuple 0 0) (Tuple 1 1)

array :: Effect Unit
array = do
  run [1, 2, 3] [1, 2, 3]
  run [1, 2] [1, 2, 3]
  run [1, 2, 3] [1, 2]
  run [[1, 2, 3], [4, 5, 6]] [[4, 5, 6], [1, 2, 3]]
  run [[1, 2, 3], [4, 5, 6]] [[1, 2, 3], [4, 5, 6]]

main :: Effect Unit
main = do
  maybe
  either
  tuple
  array
