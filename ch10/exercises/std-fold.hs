module StdFold where

import           Data.List

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

-- myAny f = myOr . (map f)
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y acc -> x == y || acc) False

-- myElem x = myAny (== x)
myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  foldr
    (\x acc ->
       if f x
         then x : acc
         else acc)
    []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

-- squishMap f = squish . (myMap f)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy cmp xs =
  foldl'
    (\acc x ->
       case acc `cmp` x of
         GT        -> acc
         otherwise -> x)
    (head xs)
    xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy cmp xs =
  foldl'
    (\acc x ->
       case acc `cmp` x of
         LT        -> acc
         otherwise -> x)
    (head xs)
    xs
