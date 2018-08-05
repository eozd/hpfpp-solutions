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

keywordSeq :: String -> String -> String
keywordSeq keyword msg = take (length msg) . concat . repeat $ keyword

shiftAmounts :: String -> [Int]
shiftAmounts = map (\c -> if isUpper c
                            then ord c - upperBegin
                            else ord c - lowerBegin)

vigenereShift :: [Int] -> String -> String
vigenereShift shiftList msg =
    map (\(c, amount) -> shift amount c) $ zip msg shiftList

-- TODO: Doesn't work with strings including chars not in ['a'..'z'] and
-- not in ['A'..'Z']

vigenere :: String -> String -> String
vigenere keyword msg =
    let ks = keywordSeq keyword msg
        shiftList = shiftAmounts ks
    in vigenereShift shiftList msg

unVigenere :: String -> String -> String
unVigenere keyword msg = 
    let ks = keywordSeq keyword msg
        shiftList = map negate $ shiftAmounts ks
    in vigenereShift shiftList msg

caesar :: Int -> String -> String
caesar n = map (shift n)

unCaesar :: Int -> String -> String
unCaesar n = map (shift . negate $ n)

