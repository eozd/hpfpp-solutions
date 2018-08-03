module ImplFromTypes where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = b == f a

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromInteger i
