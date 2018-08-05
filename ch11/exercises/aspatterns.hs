import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) ys = x `elem` ys && isSubsequenceOf xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(x:xs) -> (w, toUpper x : xs)) . words
