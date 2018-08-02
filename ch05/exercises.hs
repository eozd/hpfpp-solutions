{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where

-- 1. :t [a] is "a list whose elements are all of some type a"
-- 2. A function ([[a]] -> [a]) could "take a list of strings as argument"
-- 3. A function ([a] -> Int -> a) "takes one argument (and returns a fn (Int -> a))"
-- 4. A function ((a, b) -> a) "takes a tuple argument and returns the first value"

-- Determine the Type
-- 1.
-- (Num a => a)
a1 = (* 9) 6

-- (Num a => (a, [Char]))
b1 = head [(0, "doge"), (1, "kitten")]

-- (Integer, [Char])
c1 = head [(0 :: Integer, "doge"), (1, "kitten")]

-- Bool
d1 = if False then True else False

-- Int
e1 = length [1, 2, 3, 4, 5]

-- Bool
f1 = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- 2.
-- Num a => a
w = let x = 5; y = x + 5 in y * 10

-- 3.
-- Num a => a -> a
z y = y * 10

-- 4.
-- Fractional a => a
f4 = let x = 5; y = x + 5 in 4 / y

-- 5.
-- [Char]
f5 = let x = "Julie"; y = " <3 "; z = "Haskell" in x ++ y ++ z

-- Does it compile?
-- 1. (bigNum $ 10)
bigNum = (^) 5 $ 10
wahoo = bigNum + 10

-- 2. (no problems)
x2 = print
y2 = print "woohoo!"
z2 = x2 "hello world"

-- 3. (c = b 10)
a3 = (+)
b3 = 5
c3 = a3 10
d3 = c3 200

-- 4. (b = .. * c; c not defined)
a4 = 12 + b4
b4 = 1000

-- Describe Type
-- 2.
-- f :: zed -> Zed -> Blah
-- zed is polymorphic
-- Zed and Blah are concrete types

-- 3.
-- f :: Enum b => a -> b -> c
-- a and c are polymorphic
-- b is consrained

-- 4.
-- f :: f -> g -> C
-- f and g are polymorphic
-- C is concrete

-- Type Signature
functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
-- functionC x y = if (x > y) then True else False
functionC x y = x > y

functionS :: (a, b) -> b
functionS (x, y) = y

-- Write function from type
i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r _ = []
-- r x = x
-- r x = x ++ x
-- r x = x ++ x ++ x

co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f $ g x

a7 :: (a -> c) -> a -> a
a7 f x = x

a7' :: (a -> b) -> a -> b
a7' f = f

-- Type-Kwon-Do

-- 1.
f8 :: Int -> String
f8 = undefined

g8 :: String -> Char
g8 = undefined

h8 :: Int -> Char
h8 x = g8 $ f8 x

-- 2.
data A
data B
data C

q8 :: A -> B
q8 = undefined

w8 :: B -> C
w8 = undefined

e8 :: A -> C
e8 a = w8 $ q8 a

-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst $ g $ f x
