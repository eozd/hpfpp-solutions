import Data.List
import Data.Char

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe str = intercalate " " . reverse . go (map notThe $ words str) $ []
  where go [] acc = acc
        go (w:ws) acc = case w of Just x -> go ws (x:acc)
                                  otherwise -> go ws ("a":acc)

vowels :: String
vowels = "aoeui"

isVowel :: Char -> Bool
isVowel c = toLower c `elem` vowels

isConsonant :: Char -> Bool
isConsonant c = isLetter c && (not . isVowel)  c

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (map notThe $ words str) 0
    where go [] count = count
          go [_] count = count
          go (w1:w2:ws) count =
              case w1 of
                  Just x -> go (w2:ws) count
                  otherwise -> case w2 of
                      Just y -> if isVowel (head y)
                                    then go ws (count + 1)
                                    else go ws count
                      otherwise -> go (w2:ws) count

count :: (a -> Bool) -> [a] -> Integer
count pred = toInteger . length . (filter pred)

countVowels :: String -> Integer
countVowels = count isVowel

countConsonants :: String -> Integer
countConsonants = count isConsonant

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str = let numVowels = countVowels str
                 numConsonants = countConsonants str
             in if numVowels > numConsonants
                   then Nothing
                   else Just $ Word' str
