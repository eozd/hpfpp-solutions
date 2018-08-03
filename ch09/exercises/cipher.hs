module Cipher where

import           Data.Char

lowerBegin :: Int
lowerBegin = ord 'a'

lowerEnd :: Int
lowerEnd = ord 'z'

upperBegin :: Int
upperBegin = ord 'A'

upperEnd :: Int
upperEnd = ord 'Z'

shift :: Int -> Char -> Char
shift n ch =
  let (begin, end) =
        if isUpper ch
          then (upperBegin, upperEnd)
          else (lowerBegin, lowerEnd)
      alphabetSize = end - begin + 1
      shiftAmount = n `mod` alphabetSize
      currPos = ord ch - begin
      newPos = (currPos + shiftAmount) `mod` alphabetSize
      newChar = chr $ begin + newPos
   in newChar

-- TODO: Doesn't work with strings including chars not in ['a'..'z'] and
-- not in ['A'..'Z']
caesar :: Int -> String -> String
caesar n = map (shift n)

unCaesar :: Int -> String -> String
unCaesar n = map (shift . negate $ n)
