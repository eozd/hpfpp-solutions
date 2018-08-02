module TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3

myConcat x = x ++ " yo"
-- :t myConcat is ([Char] -> [Char])

myMult x = (x / 3) * 5
-- :t myMult is (Fractional a => a -> a)

myTake x = take x "hey you"
-- :t myTake is (Int -> [Char])

myCom x = x > (length [1..10])
-- :t myCom is (Int -> Bool)

myAlph x = x < 'z'
-- :t myAlph is (Char -> Bool)
