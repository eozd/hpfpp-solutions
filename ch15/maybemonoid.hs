import           Control.Monad
import           Data.Monoid
import           Data.Semigroup
import           Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = arbitrary >>= (\a -> elements [First' Nada, First' (Only a)])

instance Semigroup (First' a) where
  (<>) (First' Nada) snd = snd
  (<>) fst _             = fst

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
