split' :: (Eq a) => a -> [a] -> [[a]]
split' _ [] = []
split' delim xs =
    let word = takeWhile (/= delim) xs
        restWithDelim = dropWhile (/= delim) xs
        rest =
          case restWithDelim of
            [] -> []
            otherwise -> tail restWithDelim
    in word : split' delim rest

words' :: String -> [String]
words' = split' ' '

lines' :: String -> [String]
lines' = split' '\n'
