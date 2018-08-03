{-# LANGUAGE NoMonomorphismRestriction #-}

-- all functions are the same
mTh x y z = x * y * z

mTh' x y = \z -> x * y * z

mTh'' x = \y -> \z -> x * y * z

mTh''' = \x -> \y -> \z -> x * y * z

-- :t mTh 3 is still (Num a => a -> a -> a)

addOneIfOdd n =
  case odd n of
    True  -> (\x -> x + 1) n
    False -> n

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
