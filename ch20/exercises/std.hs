module StdFunctions where

import           Data.Foldable (foldMap, foldr)
import           Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (== x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs
  | null' xs = Nothing
  | otherwise =
    Just $
    foldr1
      (\x acc ->
         if x < acc
           then x
           else acc)
      xs

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs
  | null' xs = Nothing
  | otherwise =
    Just $
    foldr1
      (\x acc ->
         if x > acc
           then x
           else acc)
      xs

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t, Integral b) => t a -> b
length' = getSum . foldMap (const (Sum 1))

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap pure

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty
