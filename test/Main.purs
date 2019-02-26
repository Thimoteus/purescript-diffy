module Test.Main where

import Prelude

import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Foldable (for_, traverse_)
import Data.Generic.Rep (class Generic, from)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(Nil), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Diffy (class Diffy, atomicDiff, diffy)
import Diffy.Atomic (class Atomic, Atomizable, atomize)
import Diffy.Atomic as Atomic
import Diffy.Atomic.Generic (genericAtomize, genericCtor)
import Diffy.Callback as Callback
import Diffy.Generic (genericDiffy)
import Diffy.Index (class Indexed, Index, Indexable, index)
import Diffy.Index as Index
import Diffy.Ordered (class Ordered, Orderable, size)
import Diffy.Ordered as Ordered
import Effect (Effect)
import Effect.Console as Console
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record as Record
import Test.Coverage as Test.Coverage
import Test.Effect as Test.Effect
import Test.Writer as Test.Writer
import Type.Row (RLProxy(..))

newtype Address = Address String
newtype Name = Name String
newtype Phone = Phone Int
data Person
  = WithNumber {name :: Name, address :: Address, number :: Phone}
  | NoNumber {name :: Name, address :: Address}
newtype Phonebook = Phonebook (Array Person)

derive instance genericAddress :: Generic Address _
derive instance genericName :: Generic Name _
derive instance genericPhone :: Generic Phone _

derive instance eqAddress :: Eq Address
instance showAddress :: Show Address where
  show x = genericShow x
instance atomicAddress :: Atomic Address where
  atomize x = genericAtomize x
  ctor x = genericCtor x
instance diffAddress :: Diffy Address where
  diffy = atomicDiff

derive instance eqName :: Eq Name
instance showName :: Show Name where
  show x = genericShow x
instance atomicName :: Atomic Name where
  atomize x = genericAtomize x
  ctor x = genericCtor x
instance diffName :: Diffy Name where
  diffy = atomicDiff

derive instance eqPhone :: Eq Phone
instance showPhone :: Show Phone where
  show x = genericShow x
instance atomicPhone :: Atomic Phone where
  atomize x = genericAtomize x
  ctor x = genericCtor x
instance diffPhone :: Diffy Phone where
  diffy = atomicDiff

derive instance genericPerson :: Generic Person _
instance diffPerson :: Diffy Person where
  diffy f x y = genericDiffy f x y

derive instance genericPhonebook :: Generic Phonebook _
instance diffPhoneBook :: Diffy Phonebook where
  diffy f x y = genericDiffy f x y

pb :: Phonebook
pb = Phonebook
  [ WithNumber
      { name: Name "Me"
      , address: Address "Here"
      , number: Phone 1000000
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
      , number: Phone 1000000
      }
  , WithNumber
      { name: Name "You"
      , address: Address "There"
      , number: Phone 999999
      }
  , NoNumber
      { name: Name "You"
      , address: Address "Way over there"
      }
  ]

data Diff
  = Atomizable Atomizable Atomizable
  | Orderable Orderable Orderable
  | Indexable Indexable Indexable

writerCb :: Callback.Callback (WriterT (Array Diff) Effect)
writerCb = Callback.mk onAtom onSize onIndex
  where
    onAtom :: forall a. Atomic a => List Index -> a -> a -> WriterT (Array Diff) Effect Unit
    onAtom _path a1 a2 =
      when (atomize a1 /= atomize a2) do
        tell [Atomizable (Atomic.mk a1) (Atomic.mk a2)]
    onSize :: forall s. Ordered s => List Index -> s -> s -> WriterT (Array Diff) Effect Unit
    onSize _path l1 l2 =
      when (size l1 /= size l2) do
        tell [Orderable (Ordered.mk l1) (Ordered.mk l2)]
    onIndex :: forall i i'. Indexed i => Indexed i' => List Index -> i -> i' -> WriterT (Array Diff) Effect Unit
    onIndex _path c1 c2 =
      when (index c1 /= index c2) do
        tell [Indexable (Index.mk c1) (Index.mk c2)]

main :: Effect Unit
main = do
  Test.Effect.main
  Test.Writer.main
  Test.Coverage.main
