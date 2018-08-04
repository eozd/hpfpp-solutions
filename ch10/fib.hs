fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibs20 :: [Integer]
fibs20 = take 20 go
  where
    go = 1 : scanl (+) 1 go

fibs100 :: [Integer]
fibs100 = takeWhile (< 100) go
  where
    go = 1 : scanl (+) 1 go

factorial :: [Integer]
factorial = scanl (*) 1 [2 ..]
