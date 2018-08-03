module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where
    xLast = x `div` 100
    d = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y bool =
  case bool of
    True  -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
  | bool = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
    print ((roundTrip 4) :: Int)
    print (id 4)
