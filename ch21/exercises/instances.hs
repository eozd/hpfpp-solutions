{-# LANGUAGE FlexibleContexts #-}

module Instances where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Data.Foldable
import           Data.Monoid
import           Data.Traversable

--------------------------------------------------------------------------------
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  sequenceA (Identity applicative) = fmap Identity applicative

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--------------------------------------------------------------------------------
newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  sequenceA (Constant a) = pure (Constant a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

--------------------------------------------------------------------------------
data Optional a
  = Nada
  | Yes a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yes a) = Yes (f a)

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yes x) = f x

instance Traversable Optional where
  sequenceA Nada              = pure Nada
  sequenceA (Yes applicative) = fmap Yes applicative

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (1, Yes <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

--------------------------------------------------------------------------------
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

append' :: List a -> List a -> List a
append' Nil ys         = ys
append' (Cons x xs) ys = append' xs (Cons x ys)

instance Foldable List where
  foldMap _ Nil           = mempty
  foldMap f (Cons a rest) = (f a) <> (foldMap f rest)

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = append' <$> fmap toList x <*> sequenceA xs
    where
      toList x = Cons x Nil

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
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 <> f b2

instance Traversable (Three' a) where
  sequenceA (Three' a b1 b2) = (Three' a) <$> b1 <*> b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

--------------------------------------------------------------------------------
data S n a =
  S (n a)
    a
  deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S inner a) = S (fmap f inner) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S inner a) = foldMap f inner <> f a

instance (Traversable n) => Traversable (S n) where
  sequenceA (S inner a) = S <$> sequenceA inner <*> a

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

--------------------------------------------------------------------------------
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty               = Empty
  fmap f (Leaf a)            = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap _ Empty               = mempty
  foldMap f (Leaf a)            = f a
  foldMap f (Node left a right) = foldMap f left <> (f a) <> foldMap f right

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf a) = fmap Leaf a
  sequenceA (Node left a right) =
    Node <$> sequenceA left <*> a <*> sequenceA right

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (1, return Empty)
      , (1, Leaf <$> arbitrary)
      , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance (Eq a) => EqProp (Tree a) where
    (=-=) = eq

--------------------------------------------------------------------------------
main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Constant String (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: List (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three' String (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: S Maybe (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
