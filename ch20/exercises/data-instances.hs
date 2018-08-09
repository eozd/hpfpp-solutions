module DataInstances where

import           Data.Foldable
import           Data.Monoid

data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

data Two a b =
  Two a
      b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c =
  Three a
        b
        c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

data Three' a b =
  Three' a
         b
         b

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 <> f b2

data Four' a b =
  Four' a
        b
        b
        b

instance Foldable (Four' a) where
  foldMap f (Four' _ b1 b2 b3) = f b1 <> f b2 <> f b3

filterF' ::
     (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF' f =
  foldMap
    (\x ->
       if f x
         then pure x
         else mempty)
