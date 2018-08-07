module Listy where

import Data.Monoid
import Data.Semigroup

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)

instance Semigroup (Listy a) where
    (<>) (Listy l) (Listy r) = Listy $ l <> r

instance Monoid (Listy a) where
    mempty = Listy []
    mappend = (<>)
