module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.Maybe    (isJust)
import           Puzzle
import           RandomWord
import           System.Exit   (exitSuccess)

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle $ fmap toLower word
  runGame puzzle
