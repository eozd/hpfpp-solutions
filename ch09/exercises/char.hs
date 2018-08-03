module CharExercises where

import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capFirst :: String -> String
capFirst "" = ""
capFirst (x:xs) = toUpper x : xs

capAll :: String -> String
capAll "" = ""
capAll (x:xs) = toUpper x : capAll xs

capAndGetFirst :: String -> Maybe Char
capAndGetFirst "" = Nothing
capAndGetFirst str = Just . toUpper . head $ str
