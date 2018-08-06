module Main where

import           Cipher
import           Control.Monad   (replicateM)
import           Test.QuickCheck

caesarIdentity :: Property
caesarIdentity =
  forAll
    (arbitrary :: Gen (String, Int))
    (\(s, count) -> s == ((unCaesar count) . (caesar count) $ s))

vigenereIdentity :: Property
vigenereIdentity =
  forAll
    genVigenerePair
    (\(s, keyword) -> s == ((unVigenere keyword) . (vigenere keyword) $ s))

genVigenerePair :: Gen (String, VigenereKeyword)
genVigenerePair = do
  msg <- arbitrary
  keywordLen <- (arbitrary :: Gen Int) `suchThat` (\n -> n > 0 && n < 10)
  let arbitraryKeywordChar = arbitraryASCIIChar `suchThat` inRange
  keyword <- replicateM keywordLen arbitraryKeywordChar
  case mkVigenereKeyword keyword of
    Right vigKeyword -> return (msg, vigKeyword)

main :: IO ()
main = do
  quickCheck caesarIdentity
  quickCheck vigenereIdentity
