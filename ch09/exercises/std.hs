module MyStd where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x
    then True
    else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
  if f x
    then True
    else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
-- myElem _ [] = False
-- myElem a (x:xs) = if a == x then True else myElem a xs
myElem a xs = myAny (== a) xs

myReverse :: [a] -> [a]
myReverse xs = go xs []
  where
    go [] acc     = acc
    go (x:xs) acc = go xs (x : acc)

squish :: [[a]] -> [a]
squish [] = []
squish xs = myReverse $ go xs []
  where
    go [] acc     = acc
    go (x:xs) acc = go xs (appendAll x acc)
    appendAll [] acc     = acc
    appendAll (x:xs) acc = appendAll xs (x : acc)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . (map f)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy cmp (x:xs) = go cmp x xs
  where
    go _ currMax [] = currMax
    go cmp currMax (x:xs) =
      case x `cmp` currMax of
        LT        -> go cmp currMax xs
        otherwise -> go cmp x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy cmp (x:xs) = go cmp x xs
  where
    go _ currMin [] = currMin
    go cmp currMin (x:xs) =
      case x `cmp` currMin of
        GT        -> go cmp currMin xs
        otherwise -> go cmp x xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
