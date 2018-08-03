module TypeMatch where

import Data.List (sort)

-- 1.
-- cannot change to (i :: a); 1 requires at least Num
i :: Num a => a
i = 1

-- 2.
-- cannot change to (f :: Num a => a); 1.0 requires at least Fractional
f2 :: Float
f2 = 1.0

-- 3.
-- Fractional is OK
f3 :: Fractional a => a
f3 = 1.0

-- 4.
-- RealFrac is more general than Double; it is OK
f4 :: RealFrac a => a
f4 = 1.0

-- 5.
-- Ord a => a -> a is OK, although unnecessarily constrained
freud :: Ord a => a -> a
freud x = x

-- 6.
-- OK; id for Ints.
freud' :: Int -> Int
freud' x = x

-- 7.
-- cannot change to (a -> a); myX is Int
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- 8.
-- cannot change to (Num a => a -> a); cannot promote Int to Num

-- 9.
-- Int has Ord instance; OK.
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10.
-- young doesn't do anything Char specific; can promote to (Ord a => a)
young :: (Ord a) => [a] -> a
young xs = head (sort xs)

-- 11.
-- mySort works only with [Char]; cannot change signifier to (Ord a => [a] -> a)
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
