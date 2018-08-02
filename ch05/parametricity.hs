module Parametricity where

f :: a -> a
f x = x

f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

f3 :: a -> b -> b
f3 x y = y

f3' :: b -> a -> a
f3' x y = y

f4 :: a -> b -> a
f4 x y = x

fromIntegral' :: (Num b, Integral a) => a -> b
fromIntegral' = fromInteger . toInteger
