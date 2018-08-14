module Main where

import           Criterion.Main

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

{-# INLINE empty #-}
empty :: DList a
empty = DL id

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton a = DL $ \xs -> a : xs

{-# INLINE toList #-}
toList :: DList a -> [a]
toList (DL f) = f []

{-# INLINE cons #-}
infixr `cons`

cons :: a -> DList a -> DList a
cons x (DL f) = DL $ \xs -> x : (f xs)

{-# INLINE snoc #-}
infixl `snoc`

snoc :: DList a -> a -> DList a
snoc (DL f) x = DL $ \xs -> f (x : xs)

{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append (DL f) (DL g) = DL $ f . g

------------------------------ BENCHMARK ---------------------------------------

schleimel :: Int -> [Int]
schleimel i = go i []
  where
    go 0 acc = acc
    go n acc = go (n - 1) (acc ++ [n])

constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
  where
    go 0 acc = acc
    go n acc = go (n - 1) (snoc acc n)

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $ nf schleimel 1234
    , bench "concat DList" $ nf constructDList 1234
    ]
