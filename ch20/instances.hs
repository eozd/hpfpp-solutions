import Data.Foldable
import Data.Monoid

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

data Optional a = Nada | Yep a

instance Foldable Optional where
    foldr f z Nada = z
    foldr f z (Yep x) = f x z

    foldl f z Nada = z
    foldl f z (Yep x) = f z x

    foldMap f Nada = mempty
    foldMap f (Yep x) = f x
