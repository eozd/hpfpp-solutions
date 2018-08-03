module Apply where

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f = id
applyTimes n f = f . applyTimes (n - 1) f
