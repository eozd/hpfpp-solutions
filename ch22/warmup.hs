import Control.Applicative
import Data.Char

boop :: Integer -> Integer
boop = (*2)

doop :: Integer -> Integer
doop = (+10)

bip :: Integer -> Integer
bip = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
-- boopDoop = boop >>= (\a -> doop >>= (\b -> return (a + b)))
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

--------------------------------------------------------------------------------

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

together :: [Char] -> ([Char], [Char])
-- together = cap >>= (\a -> rev >>= (\b -> return (a, b)))
-- together = do
--     a <- cap
--     b <- rev
--     return (a, b)
-- together = (,) <$> cap <*> rev
together = liftA2 (,) cap rev
