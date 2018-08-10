import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure a = Reader $ \x -> a
  (<*>) (Reader f) (Reader ra) = Reader $ \r -> (f r) (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) (Reader ra) f =
    Reader $ \r ->
      let a = ra r
          newReader = f a
          rTob = runReader newReader
       in rTob r

instance Show (Reader r a) where
  show _ = "<function>"

instance (CoArbitrary r, Arbitrary a) => Arbitrary (Reader r a) where
  arbitrary = Reader <$> arbitrary

-- instance (Eq a, Eq r) => EqProp (Reader r a) where
--     (=-=) (Reader ra1) (Reader ra2) =
ask :: Reader a a
ask = Reader id
-- main :: IO ()
-- main = do
    -- quickBatch $ functor (undefined :: Reader Int (Int, Char, Int))
