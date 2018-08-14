module Main where

import           Criterion.Main

data Queue a = Queue
  { enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue front back) = Queue (a : front) back

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue front []) =
  let (head:newRear) = reverse front
   in Just $ (head, Queue [] newRear)
pop (Queue front (x:xs)) = Just (x, Queue front xs)

pushList :: a -> [a] -> [a]
pushList x xs = x : xs

popList :: [a] -> Maybe (a, [a])
popList [] = Nothing
popList xs = Just $ (last xs, init xs)

listBench :: Int -> [Int]
listBench iter = go iter []
  where
    go 0 q = q
    go n q = fill n q
    fill 0 q = q
    fill n q =
      let newQ = foldr pushList q [1 .. 100]
       in consume (n - 100) newQ
    consume 0 q = q
    consume n q =
      let Just (_, newQ) =
            foldr (\_ (Just (_, acc)) -> popList acc) (Just (0, q)) [1 .. 100]
       in fill (n - 100) newQ

queueBench :: Int -> Queue Int
queueBench iter = go iter (Queue [] [])
  where
    go 0 q = q
    go n q = fill n q
    fill 0 q = q
    fill n q =
      let newQ = foldr push q [1 .. 100]
       in consume (n - 100) newQ
    consume 0 q = q
    consume n q =
      let Just (_, newQ) =
            foldr (\_ (Just (_, acc)) -> pop acc) (Just (0, q)) [1 .. 100]
       in fill (n - 100) newQ

main :: IO ()
main =
  defaultMain
    [ bench "list version" $ whnf listBench 10000
    , bench "queue version" $ whnf queueBench 10000
    ]
