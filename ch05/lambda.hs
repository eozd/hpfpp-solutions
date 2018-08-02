module Lambda where

nonsense :: Bool -> Integer
nonsense True = 1452
nonsense False = 764

curriedF :: Integer -> Bool -> Integer
curriedF i b = nonsense b + i

uncurriedF :: (Integer, Bool) -> Integer
uncurriedF (i, b) = nonsense b + i

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> nonsense b + i

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> nonsense b + i

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b
