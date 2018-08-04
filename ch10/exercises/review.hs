stops :: [Char]
stops = "pbtdkg"

wovels :: [Char]
wovels = "aeiou"

triples :: [[Char]]
triples = [x : y : z : "" | x <- stops, y <- wovels, z <- stops]

triplesBeginningWithP :: [[Char]]
triplesBeginningWithP = ['p' : y : z : "" | y <- wovels, z <- stops]

nouns :: [[Char]]
nouns = ["apple", "car", "tree"]

verbs :: [[Char]]
verbs = ["run", "make", "perform", "smile"]

tripleNouns :: [[Char]]
tripleNouns = [x ++ " " ++ y ++ " " ++ z | x <- nouns, y <- verbs, z <- nouns]

-- divide (total length of all words) to (number of words) in a sentence
seekritFunc :: (Fractional a) => String -> a
seekritFunc x = (fromIntegral . sum . (map length) . words $ x) / (fromIntegral . length . words $ x)
