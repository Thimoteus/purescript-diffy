module Test.Main where

import Prelude

import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Diffy (class Diffy, diff)
import Diffy.Atomic (class Atomic, Atom)
import Diffy.Callback (DiffCb)
import Diffy.Generic (genericDiff)
import Effect (Effect)
import Effect.Console (log)

newtype Address = Address String
newtype Name = Name String
newtype Phone = Phone Int
data Person
  = WithNumber {name :: Name, address :: Address, number :: Phone}
  | NoNumber {name :: Name, address :: Address}
newtype Phonebook = Phonebook (Array Person)

derive newtype instance atomicAddress :: Atomic Address
derive newtype instance atomicName :: Atomic Name
derive newtype instance atomicNumber :: Atomic Phone

derive newtype instance diffName :: Diffy Name
derive newtype instance diffAddress :: Diffy Address
derive newtype instance diffPhone :: Diffy Phone

derive instance genericPerson :: Generic Person _
instance diffPerson :: Diffy Person where
  diff = genericDiff
derive newtype instance diffPhonebook :: Diffy Phonebook

pb :: Phonebook
pb = Phonebook
  [ WithNumber
      { name: Name "Me"
      , address: Address "Here"
      , number: Phone 0
      }
  , NoNumber
      { name: Name "You"
      , address: Address "There"
      }
  ]

pb' :: Phonebook
pb' = Phonebook
  [ WithNumber
      { name: Name "Me"
      , address: Address "Where I am"
      , number: Phone 0
      }
  , WithNumber
      { name: Name "You"
      , address: Address "There"
      , number: Phone 1
      }
  , NoNumber
      { name: Name "You"
      , address: Address "Way over there"
      }
  ]

effCb :: DiffCb Effect
effCb = {onAtom, onLength, onConstructor}
  where
  onAtom a1 a2
    | a1 == a2 = log "All good ✓"
    | otherwise =
      log $ "Atoms not equal! " <> show a1 <> " vs. " <> show a2
  onLength m n
    | m == n = log "All good ✓"
    | otherwise =
      log $ "Lengths not equal! " <> show m <> " vs. " <> show n
  onConstructor c1 c2
    | c1 == c2 = log "All good ✓"
    | otherwise =
      log $ "Constructors not equal! " <> show c1 <> " vs. " <> show c2

data Diff
  = Atomic Atom Atom
  | Length Int Int
  | Constructor String String

writerCb :: DiffCb (WriterT (Array Diff) Effect)
writerCb = {onAtom, onLength, onConstructor}
  where
    onAtom a1 a2 =
      when (a1 /= a2) do
        tell [Atomic a1 a2]
    onLength l1 l2 =
      when (l1 /= l2) do
        tell [Length l1 l2]
    onConstructor c1 c2 =
      when (c1 /= c2) do
        tell [Constructor c1 c2]

main :: Effect Unit
main = do
  diff effCb pb pb'
  arr <- execWriterT (diff writerCb pb pb')
  for_ arr case _ of
    Atomic a1 a2 -> log $
      "Atoms different: " <> show a1 <> " vs. " <> show a2
    Length l1 l2 -> log $
      "Lengths different: " <> show l1 <> " vs. " <> show l2
    Constructor c1 c2 -> log $
      "Constructors different: " <> show c1 <> " vs. " <> show c2