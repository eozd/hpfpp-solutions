module Main where

import           Control.Monad              (forever)
import           Control.Monad.Trans.Except
import           Data.Char
import           Data.List                  (find)
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure, exitSuccess)
import           System.IO                  (hGetLine, hPutStrLn, hWaitForInput,
                                             stderr, stdin, stdout)
import           System.IO.Error            (catchIOError)

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

mkVigenereKeyword :: String -> ExceptT String IO VigenereKeyword
mkVigenereKeyword "" =
  ExceptT $ return (Left "Vigenere keyword must be non-empty")
mkVigenereKeyword str =
  ExceptT $
  case find (not . inRange) str of
    Just _ ->
      return
        (Left
           "Vigenere keyword can only contain lower and upper case ASCII letters")
    Nothing -> return (Right $ VigenereKeyword str)

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

getTwoArgs :: [String] -> ExceptT String IO [String]
getTwoArgs args@[key, mode] = ExceptT $ return (Right args)
getTwoArgs _ =
  ExceptT $ return (Left "Arguments must be in the format: <key> <mode>")

getCipherFun ::
     String -> ExceptT String IO (VigenereKeyword -> String -> String)
getCipherFun "-d" = ExceptT $ return (Right unVigenere)
getCipherFun "-e" = ExceptT $ return (Right vigenere)
getCipherFun mode =
  ExceptT $ return (Left $ "Unexpected mode argument: " ++ mode)

main :: IO ()
main = do
  args <- getArgs
  progVars <-
    runExceptT $ do
      [keyStr, mode] <- getTwoArgs args
      cipherFun <- getCipherFun mode
      key <- mkVigenereKeyword keyStr
      return (cipherFun, key)
  case progVars of
    Left err -> do
      putStrLn err
      exitFailure
    Right (cipherFun, key) ->
      forever $ do
        inputAvailable <- hWaitForInput stdin 5000
        if inputAvailable
          then do
            line <- catchIOError (hGetLine stdin) (\_ -> exitSuccess)
            hPutStrLn stdout (cipherFun key line)
          else do
            hPutStrLn stderr "Timeout. Exiting..."
            exitFailure
