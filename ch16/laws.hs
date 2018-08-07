import           Test.QuickCheck
import           Test.QuickCheck.Function

--------------------------------------------------------------------------------
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity functor = functor == fmap id functor

functorCompose :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) functor =
  fmap (g . f) functor == (fmap g . fmap f $ functor)

--------------------------------------------------------------------------------
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= (\a -> return $ Identity a)

--------------------------------------------------------------------------------
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = arbitrary >>= (\(a1, a2) -> return $ Pair a1 a2)

--------------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = arbitrary >>= (\(a, b) -> return $ Two a b)
--------------------------------------------------------------------------------
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = arbitrary >>= (\(a, b1, b2) -> return $ Three' a b1 b2)
--------------------------------------------------------------------------------
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = arbitrary >>= (\(a1, a2, a3, b) -> return $ Four' a1 a2 a3 b)
--------------------------------------------------------------------------------
main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck
    (functorCompose :: Fun Int String -> Fun String Char -> Identity Int -> Bool)
    ---------
  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck
    (functorCompose :: Fun Int String -> Fun String Char -> Pair Int -> Bool)
    ---------
  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck
    (functorCompose :: Fun Int String -> Fun String Char -> Two String Int -> Bool)
    ---------
  quickCheck (functorIdentity :: Three' String Int -> Bool)
  quickCheck
    (functorCompose :: Fun Int String -> Fun String Char -> Three' String Int -> Bool)
    ---------
  quickCheck (functorIdentity :: Four' String Int -> Bool)
  quickCheck
    (functorCompose :: Fun Int String -> Fun String Char -> Four' String Int -> Bool)
