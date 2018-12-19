module Diffy.Callback where

import Prelude

import Diffy.Atomic (Atom)

type DiffCb m =
  { onAtom :: Atom -> Atom -> m Unit
  , onLength :: Int -> Int -> m Unit
  , onConstructor :: String -> String -> m Unit
  }