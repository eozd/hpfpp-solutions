module Exercises where

awesome = ["Papuchon", "curry", ":)"]

alsoAwesome = ["Quake", "The Simons"]

allAwesome = [awesome, alsoAwesome]

-- 1.
len :: (Num n) => [a] -> n
len []     = 0
len (x:xs) = 1 + len xs

-- 2.
a2 = len [1, 2, 3, 4, 5] == 5

b2 = len [(1, 2), (3, 4), (5, 6)] == 3

c2 = len allAwesome == 2

d2 = length (concat allAwesome) == 5

-- 3.
-- length from Prelude returns Int --> (/) requires two Fractionals.
-- need to use genericLength which is same as len above
-- 4.
-- 6 `div` length [1..3]
-- 5.
-- :t (2 + 3 == 5) is Bool
-- 6.
-- :t (let x = 5 in x + 3 == 5) is Bool
-- 7.
a7 = True == (length allAwesome == 2)

-- b7 compile error (list of different types)
c7 = 5 == (length allAwesome + length awesome)

d7 = False == ((8 == 8) && ('b' < 'a'))

-- e7 compile error ( (&&) :: Bool -> Bool -> Bool)
-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == reverse list

-- 9.
myAbs :: Integer -> Integer
myAbs a =
  if a < 0
    then (-a)
    else a

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- 11.
x = (+)

f1 xs = w `x` 1
  where
    w = length xs

-- 12.
-- \x -> x
-- 13.
-- \(x:xs) -> x
-- 14.
f2 :: (a, b) -> a
f2 (a, b) = a
-- 15.
-- :t show is (Show a => a -> String)
-- 16.
-- :t (==) is (Eq a => a -> a -> Bool)
-- 17.
-- :t fst is ((a, b) -> a)
-- 18.
-- :t (+) is (Num a => a -> a -> a)
