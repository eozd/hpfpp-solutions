module ListyInstances where

import Listy
import Data.Monoid
import Data.Semigroup

instance Semigroup (Listy a) where
    (<>) (Listy l) (Listy r) = Listy $ l <> r

instance Monoid (Listy a) where
    mempty = Listy []
    mappend = (<>)
