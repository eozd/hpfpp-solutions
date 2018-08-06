module WordNumber where

import           Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n = reverse $ go n
  where
    go d
      | d < 10 = [d]
      | otherwise = d `mod` 10 : go (d `div` 10)

wordNumber :: Int -> String
wordNumber n =
  let res = intersperse "-" . map digitToWord . digits $ abs n
      minusHandled =
        if n < 0
          then "minus-" : res
          else res
   in concat minusHandled
