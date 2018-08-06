module Puzzle where

import           Control.Monad (forever)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)

data Puzzle = Puzzle
  { wordToGuess      :: String
  , discoveredChars  :: [Maybe Char]
  , correctGuesses   :: [Char]
  , incorrectGuesses :: [Char]
  }

instance Show Puzzle where
  show (Puzzle _ discovered _ incorrect) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
    " Incorrect guesses so far: " ++ incorrect

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (replicate (length word) Nothing) [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ correct incorrect) c =
  c `elem` correct || c `elem` incorrect

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered correct incorrect) c =
  Puzzle word newDiscovered newCorrect newIncorrect
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newDiscovered = zipWith (zipper c) word discovered
    (newCorrect, newIncorrect) =
      if not (c `elem` correct) && not (c `elem` incorrect)
        then if c `elem` word
               then (c : correct, incorrect)
               else (correct, c : incorrect)
        else (correct, incorrect)

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
gameOver (Puzzle word _ _ incorrect) =
  if (length incorrect > 7)
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
