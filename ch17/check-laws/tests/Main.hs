module Main where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck          (Arbitrary, Gen, arbitrary, elements,
                                           suchThat)
import           Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import           Test.QuickCheck.Classes

--------------------------------------------------------------------------------
data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = elements [Fools, Twoo]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend = (<>)

instance EqProp Bull where
  (=-=) = eq

--------------------------------------------------------------------------------
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

append' :: List a -> List a -> List a
append' Nil ys            = ys
append' (Cons x restX) ys = Cons x (append' restX ys)

fold' :: (a -> b -> b) -> b -> List a -> b
fold' _ z Nil            = z
fold' f z (Cons x restX) = f x (fold' f z restX)

concat' :: List (List a) -> List a
concat' = fold' append' Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . (fmap f)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil            = Nil
  (<*>) Nil _            = Nil
  (<*>) funcList valList = flatMap (<$> valList) funcList

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = arbitrary `suchThat` (\xs -> length xs < 10) >>= listToOurList
    where
      listToOurList :: [a] -> Gen (List a)
      listToOurList = return . (foldr (\x acc -> Cons x acc) Nil)

instance Eq a => EqProp (List a) where
  (=-=) = eq

--------------------------------------------------------------------------------
take' :: Int -> List a -> List a
take' n list
  | n < 0 = Nil
  | otherwise = go n list
  where
    go 0 _           = Nil
    go _ Nil         = Nil
    go n (Cons x xs) = Cons x $ go (n - 1) xs

repeat' :: a -> List a
repeat' x = Cons x $ repeat' x

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  (=-=) xs ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) =
    let y = f x
        (ZipList' ys) = (ZipList' fs) <*> (ZipList' xs)
     in ZipList' $ Cons y ys

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = arbitrary >>= (return . ZipList')

--------------------------------------------------------------------------------
data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (<*>) (Success f) (Success x)   = Success (f x)
  (<*>) (Failure e) (Success _)   = Failure e
  (<*>) (Success _) (Failure e)   = Failure e
  (<*>) (Failure e1) (Failure e2) = Failure (e1 <> e2)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = arbitrary >>= (\(e, a) -> elements [Failure e, Success a])

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) (Success a1) (Success a2) = a1 `eq` a2
  (=-=) (Failure e1) (Failure e2) = e1 `eq` e2
  (=-=) _ _                       = False `eq` True -- how to write False here?

--------------------------------------------------------------------------------
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f1 f2) (Pair a1 a2) = Pair (f1 a1) (f2 a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = arbitrary >>= (\(a1, a2) -> return $ Pair a1 a2)

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

--------------------------------------------------------------------------------
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a1 f) (Two a2 b) = Two (a1 <> a2) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = arbitrary >>= (\(a, b) -> return $ Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

--------------------------------------------------------------------------------
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a1 f1 f2) (Three' a2 b1 b2) = Three' (a1 <> a2) (f1 b1) (f2 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = arbitrary >>= (\(a, b1, b2) -> return $ Three' a b1 b2)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

--------------------------------------------------------------------------------
main :: IO ()
main
  -- quickBatch (monoid Twoo)
  -- quickBatch $ applicative (undefined :: [(String, Char, Int)])
  -- quickBatch $ applicative (undefined :: List (String, Char, Int))
  -- quickBatch $ applicative (undefined :: ZipList' (String, Char, Int))
  -- quickBatch $
  --   applicative (undefined :: Validation [String] (String, Char, Int))
 = do
  quickBatch $ applicative (undefined :: Pair (String, Char, Int))
  quickBatch $ applicative (undefined :: Two [String] (String, Char, Int))
  quickBatch $ applicative (undefined :: Three' [String] (String, Char, Int))
