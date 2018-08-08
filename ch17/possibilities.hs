import Control.Applicative (liftA3)

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aoeui"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

res = combos stops vowels stops
