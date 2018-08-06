module TestPuzzle
  ( testPuzzle
  ) where

import           Puzzle
import           Test.Hspec

wordToTest :: String
wordToTest = "instant"

puzzleToTest :: Puzzle
puzzleToTest = freshPuzzle wordToTest

startedPuzzle :: Puzzle
startedPuzzle =
  let (Puzzle word discovered correct _) = puzzleToTest
   in Puzzle word discovered correct ['y', 'k']

test_fillInCharacter :: IO ()
test_fillInCharacter =
  hspec $ do
    describe "Original word never changes" $ do
      it "Correct guess" $ do
        let (Puzzle newWord _ _ _) = fillInCharacter puzzleToTest 't'
        newWord `shouldBe` wordToTest
      it "Incorrect guess" $ do
        let (Puzzle newWord _ _ _) = fillInCharacter puzzleToTest 'y'
        newWord `shouldBe` wordToTest
    describe "Correct Guess" $ do
      let (Puzzle _ newDiscovered newCorrectGuesses newIncorrectGuesses) =
            fillInCharacter puzzleToTest 't'
      it "Discovered chars are updated" $ do
        newDiscovered `shouldBe`
          [Nothing, Nothing, Nothing, Just 't', Nothing, Nothing, Just 't']
      it "Correct guesses are updated" $ do newCorrectGuesses `shouldBe` ['t']
      it "Incorrect guesses are unchanged" $ do
        newIncorrectGuesses `shouldBe` []
    describe "Incorrect Guess" $ do
      let (Puzzle _ newDiscovered newCorrectGuesses newIncorrectGuesses) =
            fillInCharacter puzzleToTest 'C'
      it "Discovered is the same" $ do
        newDiscovered `shouldBe`
          [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
      it "Correct guesses are unchanged" $ do newCorrectGuesses `shouldBe` []
      it "Incorrect guesses are updated" $ do
        newIncorrectGuesses `shouldBe` ['C']
    describe "Already Guessed" $ do
      let (Puzzle _ oldDiscovered oldCorrectGuesses oldIncorrectGuesses) =
            startedPuzzle
          (Puzzle _ newDiscovered newCorrectGuesses newIncorrectGuesses) =
            fillInCharacter startedPuzzle 'y'
      it "Discovered is the same" $ do newDiscovered `shouldBe` oldDiscovered
      it "Correct guesses are unchanged" $ do
        newCorrectGuesses `shouldBe` oldCorrectGuesses
      it "Incorrect guesses are unchanged" $ do
        newIncorrectGuesses `shouldBe` oldIncorrectGuesses

testPuzzle :: IO ()
testPuzzle = do
  test_fillInCharacter
  -- put the rest of the test functions
