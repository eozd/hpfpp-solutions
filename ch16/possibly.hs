data Possibly a = Nope | Yes a deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor Possibly where
    fmap _ Nope = Nope
    fmap f (Yes a) = Yes (f a)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance (Functor f) => Functor (Wrap f) where
    fmap g (Wrap innerFunctor) = Wrap (fmap g innerFunctor)
