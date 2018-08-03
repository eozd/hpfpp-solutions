module TypeCheck where

-- 1.
-- doesn't type check; Person has no Show instance
data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
-- doesn't type check; Mood has no Eq instance
data Mood = Blah | Woot deriving Show

settleDown x = if x == Woot then Blah else x

-- 3.
--   a. :t settleDown is Mood -> Mood
--   b. settleDown doesn't accept (Num a => a) types
--   c. Mood has no Ord instance

-- 4.
-- typechecks; subject, verb, object are all aliases for String, meaning they
-- all have Eq and Show instances. Therefore, Sentence automatically have
-- Eq and Show instances, as well.
--
-- :t s1 is [Char] -> Sentence
-- :t s2 is Sentence
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- doesn't typecheck; Papu expects Rocks and Yeah datatypes
phew = Papu "chases" True

-- typechecks
truth = Papu (Rocks "aboecr") (Yeah True)

-- typechecks; all types have necessary Eq instance
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- doesn't typecheck; no Ord instance
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
