sumN :: (Eq a, Num a) => a -> a
sumN 1 = 1
sumN n = n + sumN (n - 1)

mult :: (Integral a) => a -> a -> a
mult _ 0 = 0
mult 0 _ = 0
mult a b = a + mult a (b - 1)

mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11
