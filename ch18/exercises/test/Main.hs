module Main where

import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

--------------------------------------------------------------------------------
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

--------------------------------------------------------------------------------
data PhhEither b a
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a)  = Left' (f a)

instance Applicative (PhhEither b) where
  pure a = Left' a
  (<*>) (Left' f) (Left' a) = Left' (f a)
  (<*>) (Right' b) _        = Right' b
  (<*>) _ (Right' b)        = Right' b

instance Monad (PhhEither b) where
  return = pure
  (>>=) (Left' a) f  = f a
  (>>=) (Right' b) _ = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhEither b a) where
  arbitrary = arbitrary >>= (\(a, b) -> elements [Left' a, Right' b])

instance (Eq a, Eq b) => EqProp (PhhEither b a) where
  (=-=) = eq

--------------------------------------------------------------------------------
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= (\a -> return $ Identity a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--------------------------------------------------------------------------------
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

append' :: List a -> List a -> List a
append' Nil ys         = ys
append' (Cons x xs) ys = Cons x (append' xs ys)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _          = Nil
  (<*>) _ Nil          = Nil
  (<*>) (Cons f fs) xs = append' (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil _         = Nil
  (>>=) (Cons x xs) f = append' (f x) (xs >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    arbitrary `suchThat` (\xs -> length xs < 10) >>= return . listToOurList
    where
      listToOurList :: [a] -> List a
      listToOurList []     = Nil
      listToOurList (x:xs) = Cons x (listToOurList xs)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

--------------------------------------------------------------------------------
j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
-- l1 = liftM
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
-- meh (x:xs) f = f x >>= (\y -> meh xs f >>= (\ys -> return $ y : ys))
meh (x:xs) f = do
  y <- f x
  ys <- meh xs f
  return $ y : ys

flipType :: (Monad m) => [m a] -> m [a]
flipType mxs = meh mxs id

--------------------------------------------------------------------------------
main :: IO ()
main = do
  quickBatch $ functor (undefined :: Nope (String, Char, Int))
  quickBatch $ applicative (undefined :: Nope (String, Char, Int))
  quickBatch $ monad (undefined :: Nope (String, Char, Int))
  -----------
  quickBatch $ functor (undefined :: PhhEither String (String, Char, Int))
  quickBatch $ applicative (undefined :: PhhEither String (String, Char, Int))
  quickBatch $ monad (undefined :: PhhEither String (String, Char, Int))
  -----------
  quickBatch $ functor (undefined :: Identity (String, Char, Int))
  quickBatch $ applicative (undefined :: Identity (String, Char, Int))
  quickBatch $ monad (undefined :: Identity (String, Char, Int))
  -----------
  quickBatch $ functor (undefined :: List (String, Char, Int))
  quickBatch $ applicative (undefined :: List (String, Char, Int))
  quickBatch $ monad (undefined :: List (String, Char, Int))
