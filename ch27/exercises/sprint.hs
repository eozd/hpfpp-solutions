-- 1.
-- let x = 1
-- :sprint x --> x = _

-- 2.
-- let x = ['1']
-- :sprint x --> x = "1" -- because x :: [Char] and Char is primitive

-- 3.
-- let x = [1]
-- :sprint x --> x = _ -- 1 is polymorphic; hence, data ctor stays in WHNF

-- 4.
-- let x = 1 :: Int
-- :sprint x --> x = _ -- no ctor, x is not used

-- 5.
-- let f = \x -> x
-- let x = f 1
-- :sprint f --> f = _ -- f is not used
-- :sprint x --> x = _ -- x requires function call

-- 6.
-- let f :: Int -> Int; f = \x -> x
-- let x = f 1
-- :sprint f --> f = _ -- f is not used
-- :sprint x --> x = _ -- x requires function call
