import GHC.Arr

-- 1. Not possible to write a Functor instance.
-- No value inside the structure to map over
-- data Bool' = False' | True'

-- 2. Possible; we can map over values of type a
data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a) = True' (f a)

-- 3. Possible; we can map over values of type a
data BoolAndMaybeSomethingElse a = Falsish' | Truish' a

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish' = Falsish'
    fmap f (Truish' a) = Truish' (f a)

-- 4. Not possible; :k Mu is (* -> *) -> * which cannot be put into * -> * form
newtype Mu f = InF { outF :: f (Mu f) }

-- 5. Not possible; :k D is * which cannot be put into * -> * form
data D = D (Array Word Word) Int Int
