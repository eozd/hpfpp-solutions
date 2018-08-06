module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in (l > minWordLength) && (l < maxWordLength)

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIdx <- randomRIO (0, length wl - 1)
  return $ wl !! randomIdx

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle
  { wordToGuess      :: String
  , discoveredChars  :: [Maybe Char]
  , correctGuesses   :: [Char]
  , incorrectGuesses :: [Char]
  }

instance Show Puzzle where
  show (Puzzle _ discovered _ incorrectGuesses) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
    " Incorrect guesses so far: " ++ incorrectGuesses

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (replicate (length word) Nothing) [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ correctGuesses incorrectGuesses) c =
  c `elem` correctGuesses || c `elem` incorrectGuesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered correctGuesses incorrectGuesses) c =
  Puzzle word newDiscovered newCorrectGuesses newIncorrectGuesses
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newDiscovered = zipWith (zipper c) word discovered
    (newCorrectGuesses, newIncorrectGuesses) =
      if c `elem` word
        then (c : correctGuesses, incorrectGuesses)
        else (correctGuesses, c : incorrectGuesses)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character is in the word, filling accordingly..."
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "This character is not in the word, try again."
      return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ _ incorrectGuesses) =
  if (length incorrectGuesses > 7)
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ word
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _ _) =
  if all isJust discovered
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle $ fmap toLower word
  runGame puzzle
