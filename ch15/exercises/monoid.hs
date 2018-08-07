import           Data.Monoid
import           Data.Semigroup
import           Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary, elements,
                                  quickCheck)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = (mempty <> m) == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = (m <> mempty) == m

--------------------------------------------------------------------------------
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

--------------------------------------------------------------------------------
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity $ a <> b

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= (\a -> return $ Identity a)

--------------------------------------------------------------------------------
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a1 b1) (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- Three and Four is basically the same
--------------------------------------------------------------------------------
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _                             = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

--------------------------------------------------------------------------------
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _ _                               = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

--------------------------------------------------------------------------------
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Fst a) (Fst b) = Fst b
  (<>) (Snd a) _       = Snd a
  (<>) _ (Snd b)       = Snd b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = arbitrary >>= (\(a, b) -> elements [Fst a, Snd b])

--------------------------------------------------------------------------------
newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

instance Show (Combine a b) where
  show (Combine _) = "a -> b"

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (\x -> f x <> g x)

instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\_ -> mempty)
    mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = arbitrary >>= (\f -> return $ Combine f)

combineAssoc ::
     (Eq b, Semigroup b)
  => Combine a b
  -> Combine a b
  -> Combine a b
  -> a
  -> Bool
combineAssoc x y z a =
  (unCombine (x <> (y <> z)) $ a) == (unCombine ((x <> y) <> z) $ a)

combineLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineLeftIdentity x a = (unCombine (mempty <> x) $ a) == (unCombine x $ a)

combineRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineRightIdentity x a = (unCombine (x <> mempty) $ a) == (unCombine x $ a)

--------------------------------------------------------------------------------
newtype Comp a = Comp
  { unComp :: (a -> a)
  }

instance Show (Comp a) where
  show (Comp _) = "a -> a"

instance (Semigroup a) => Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp $ f . g

instance (Monoid a) => Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = arbitrary >>= (\f -> return $ Comp f)

compAssoc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc x y z a = (unComp (x <> (y <> z)) $ a) == (unComp ((x <> y) <> z) $ a)

compLeftIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
compLeftIdentity x a = (unComp (mempty <> x) $ a) == (unComp x $ a)

compRightIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
compRightIdentity x a = (unComp (x <> mempty) $ a) == (unComp x $ a)

--------------------------------------------------------------------------------
data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success b1) (Success b2) = Success b1
  (<>) (Failure a) (Success b)   = Failure a
  (<>) (Success b) (Failure a)   = Failure a
  (<>) (Failure a1) (Failure a2) = Failure $ a1 <> a2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = arbitrary >>= (\(a, b) -> elements [Failure a, Success b])

--------------------------------------------------------------------------------
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight (Success b1)) (AccumulateRight (Success b2)) =
    AccumulateRight $ Success $ b1 <> b2
  (<>) (AccumulateRight (Failure a)) _ = AccumulateRight $ Failure a
  (<>) _ (AccumulateRight (Failure a)) = AccumulateRight $ Failure a

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary =
    arbitrary >>=
    (\(a, b) ->
       elements [AccumulateRight (Failure a), AccumulateRight (Success b)])

--------------------------------------------------------------------------------
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (Success b1)) (AccumulateBoth (Success b2)) =
    AccumulateBoth $ Success $ b1 <> b2
  (<>) (AccumulateBoth (Failure a1)) (AccumulateBoth (Failure a2)) =
    AccumulateBoth $ Failure $ a1 <> a2
  (<>) _ (AccumulateBoth (Failure a)) = AccumulateBoth $ Failure a
  (<>) (AccumulateBoth (Failure a)) _ = AccumulateBoth $ Failure a

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary =
    arbitrary >>=
    (\(a, b) ->
       elements [AccumulateBoth (Failure a), AccumulateBoth (Success b)])

--------------------------------------------------------------------------------
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
------------
  quickCheck
    (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
------------
  quickCheck
    (semigroupAssoc :: Two [Integer] String -> Two [Integer] String -> Two [Integer] String -> Bool)
  quickCheck (monoidLeftIdentity :: Two [Integer] String -> Bool)
  quickCheck (monoidRightIdentity :: Two [Integer] String -> Bool)
------------
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
------------
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
------------
  quickCheck
    (semigroupAssoc :: Or Char Integer -> Or Char Integer -> Or Char Integer -> Bool)
------------
  quickCheck
    (combineAssoc :: Combine Integer String -> Combine Integer String -> Combine Integer String -> Integer -> Bool)
  quickCheck (combineLeftIdentity :: Combine Integer String -> Integer -> Bool)
  quickCheck (combineRightIdentity :: Combine Integer String -> Integer -> Bool)
------------
  quickCheck
    (compAssoc :: Comp [Integer] -> Comp [Integer] -> Comp [Integer] -> [Integer] -> Bool)
  quickCheck (compLeftIdentity :: Comp [Integer] -> [Integer] -> Bool)
  quickCheck (compRightIdentity :: Comp [Integer] -> [Integer] -> Bool)
------------
  quickCheck
    (semigroupAssoc :: Validation [String] Integer -> Validation [String] Integer -> Validation [String] Integer -> Bool)
------------
  quickCheck
    (semigroupAssoc :: AccumulateRight [String] String -> AccumulateRight [String] String -> AccumulateRight [String] String -> Bool)
------------
  quickCheck
    (semigroupAssoc :: AccumulateBoth [String] String -> AccumulateBoth [String] String -> AccumulateBoth [String] String -> Bool)
