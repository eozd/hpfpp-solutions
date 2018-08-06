module Cipher
  ( inRange
  , VigenereKeyword
  , mkVigenereKeyword
  , vigenere
  , unVigenere
  , caesar
  , unCaesar
  ) where

import           Data.Char
import           Data.List (find)

lowerBegin :: Int
lowerBegin = ord 'a'

lowerEnd :: Int
lowerEnd = ord 'z'

upperBegin :: Int
upperBegin = ord 'A'

upperEnd :: Int
upperEnd = ord 'Z'

inRange :: Char -> Bool
inRange ch =
  let lowerCh = toLower ch
      ascii = ord lowerCh
   in (ascii >= lowerBegin && ascii <= lowerEnd)

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
shiftAmounts =
  map
    (\c ->
       if isUpper c
         then ord c - upperBegin
         else ord c - lowerBegin)

newtype VigenereKeyword =
  VigenereKeyword String
  deriving (Eq, Show)

mkVigenereKeyword :: String -> Either String VigenereKeyword
mkVigenereKeyword "" = Left "Vigenere keyword must be non-empty"
mkVigenereKeyword str =
  case find (not . inRange) str of
    Just _ ->
      Left
        "Vigenere keyword can only contain lower and upper case ASCII letters"
    Nothing -> Right $ VigenereKeyword str

vigenereShift :: [Int] -> String -> String
vigenereShift shiftList msg =
  map
    (\(c, amount) ->
       if inRange c
         then shift amount c
         else c) $
  zip msg shiftList

-- TODO: Doesn't work with strings including chars not in ['a'..'z'] and
-- not in ['A'..'Z']
vigenere :: VigenereKeyword -> String -> String
vigenere (VigenereKeyword keyword) msg =
  let ks = keywordSeq keyword msg
      shiftList = shiftAmounts ks
   in vigenereShift shiftList msg

unVigenere :: VigenereKeyword -> String -> String
unVigenere (VigenereKeyword keyword) msg =
  let ks = keywordSeq keyword msg
      shiftList = map negate $ shiftAmounts ks
   in vigenereShift shiftList msg

caesar :: Int -> String -> String
caesar n =
  map
    (\c ->
       if inRange c
         then shift n c
         else c)

unCaesar :: Int -> String -> String
unCaesar n =
  map
    (\c ->
       if inRange c
         then shift (-n) c
         else c)
