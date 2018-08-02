-- 1.
f :: a -> a -> a -> a
f = undefined

x :: Char
x = undefined
-- :t f x is (Char -> Char -> Char)

-- 2.
g :: a -> b -> c -> b
g = undefined
-- :t g 0 'c' "woot" is (Char)

-- 3. 4.
h :: (Num a, Num b) => a -> b -> b
h = undefined
-- :t h 1.0 2 is (Num b => b)
-- :t h 1 (5.5 :: Double) is (Double)

-- 5. 6.
jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined
-- :t jackal "keyboard" "has oo" is ([Char])
-- :t jackal "keyboard" is (Eq b => b -> [Char])

-- 7. 8. 9.
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined
-- :t kessel 1 2 is (Ord a, Num a => a)
-- :t kessel 1 (2 :: Integer) is (Ord a, Num a => a)
-- :t kessel (1 :: Integer) 2 is (Integer)
