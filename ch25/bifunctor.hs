import Data.Bifunctor

data Deux a b = Deux a b

instance Bifunctor Deux where
    first f (Deux a b) = Deux (f a) b
    second f (Deux a b) = Deux a (f b)

data Const a b = Const a

instance Bifunctor Const where
    first f (Const a) = Const (f a)
    second _ (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    first f (Drei a b c) = Drei a (f b) c
    second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    first f (SuperDrei a b) = SuperDrei a (f b)
    second _ (SuperDrei a b) = SuperDrei a b

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    first _ (SemiDrei a) = SemiDrei a
    second _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    first f (Quadzzz a b c d) = Quadzzz a b (f c) d
    second f (Quadzzz a b c d) = Quadzzz a b c (f d)

data Validation a b = Failure a | Success b

instance Bifunctor Validation where
    first f (Failure a) = Failure (f a)
    first _ (Success b) = Success b
    second _ (Failure a) = Failure a
    second f (Success b) = Success (f b)
