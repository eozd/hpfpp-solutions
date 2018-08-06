module CipherInteractive where

import           Control.Monad (forever)
import           Data.Char
import           System.IO

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
shiftAmounts =
  map
    (\c ->
       if isUpper c
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

caesarInteractive :: IO ()
caesarInteractive = do
    putStr "Message to encrypt: "
    msg <- getLine
    putStr "Encryption shift amount: "
    shiftAmountStr <- getLine
    let shiftAmount = read shiftAmountStr :: Int
    putStrLn $ "Encrypted message: " ++ caesar shiftAmount msg

unCaesarInteractive :: IO ()
unCaesarInteractive = do
    putStr "Message to decrypt: "
    msg <- getLine
    putStr "Encryption shift amount: "
    shiftAmountStr <- getLine
    let shiftAmount = read shiftAmountStr :: Int
    putStrLn $ "Decrypted message: " ++ unCaesar shiftAmount msg

vigenereInteractive :: IO ()
vigenereInteractive = do
  putStr "Message to encrypt: "
  msg <- getLine
  putStr "Encryption keyword: "
  keyword <- getLine
  putStrLn $ "Encrypted message: " ++ vigenere keyword msg

unVigenereInteractive :: IO ()
unVigenereInteractive = do
    putStr "Message to decrypt: "
    msg <- getLine
    putStr "Encryption keyword: "
    keyword <- getLine
    putStrLn $ "Decrypted message: " ++ unVigenere keyword msg

pickCipher :: IO (IO ())
pickCipher = do
  putStr "Enter a cipher name: "
  cipherName <- getLine
  putStr "Encrypt or Decrypt: "
  encrType <- getLine
  case (map toLower cipherName, map toLower encrType) of
    ("caesar", "encrypt") -> return caesarInteractive
    ("caesar", "decrypt") -> return unCaesarInteractive
    ("vigenere", "encrypt") -> return vigenereInteractive
    ("vigenere", "decrypt") -> return unVigenereInteractive
    otherwise -> do
      putStrLn "Incorrect input. Enter (caesar/vigenere) and (encrypt/decrypt)"
      return $ return ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    cipherFunc <- pickCipher
    cipherFunc
    putStrLn ""
