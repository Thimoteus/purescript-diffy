module Diffy.Index where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

data Constructor = Data String | Type String

derive instance eqConstructor :: Eq Constructor

instance showConstructor :: Show Constructor where
  show = case _ of
    Data c -> "(Data Constructor " <> c <> ")"
    Type c -> "(Type Constructor " <> c <> ")"

data Index
  = Place Int
  | Label String
  | Constructor Constructor

derive instance eq :: Eq Index

instance show :: Show Index where
  show = case _ of
    Place i -> "(Place " <> show i <> ")"
    Label l -> "(Label " <> l <> ")"
    Constructor c -> show c

display :: Index -> String
display i = "at " <> case i of
  Place n -> "place " <> show n
  Label s -> "row label " <> show s
  Constructor c -> case c of
    Data s -> "data constructor " <> show s
    Type s -> "type constructor " <> show s

class Indexed i where
  index :: i -> Index

instance int :: Indexed Int where
  index = Place

instance string :: Indexed String where
  index = Label

instance constructor :: Indexed Constructor where
  index = Constructor

data DCProxy (sym :: Symbol) = DCProxy

data TCProxy (sym :: Symbol) = TCProxy

instance dcProxy :: IsSymbol dcproxy => Indexed (DCProxy dcproxy) where
  index _ = Constructor (Data (reflectSymbol (SProxy :: SProxy dcproxy)))

instance tcProxy :: IsSymbol tcproxy => Indexed (TCProxy tcproxy) where
  index _ = Constructor (Type (reflectSymbol (SProxy :: SProxy tcproxy)))

newtype Indexable = Indexable (forall r. (forall i. Indexed i => i -> r) -> r)

mk :: forall i. Indexed i => i -> Indexable
mk i = Indexable (_ $ i)

instance indexable :: Indexed Indexable where
  index (Indexable run) = run index