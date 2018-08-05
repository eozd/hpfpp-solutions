import Data.Char
import Data.List

split' :: (Eq a) => a -> [a] -> [[a]]
split' _ [] = [[]]
split' delim [x] 
  | delim /= x = [[x]]
split' delim xs =
    let word = takeWhile (/= delim) xs
        restWithDelim = dropWhile (/= delim) xs
        rest =
          case restWithDelim of
            [] -> []
            otherwise -> tail restWithDelim
    in word : split' delim rest


capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph paragraph =
    let sentences = split' '.' paragraph
        wordsList = map words sentences
        capitalWordsList = map (\ws -> if null ws then ws else capitalizeWord (head ws) : (tail ws)) wordsList
        capitalSentencesList = map (intercalate " ") capitalWordsList
        capitalSentence = intercalate ". " $ capitalSentencesList
    in capitalSentence
