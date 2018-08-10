module RandomExample where

import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

instance Enum Die where
  fromEnum DieOne   = 1
  fromEnum DieTwo   = 2
  fromEnum DieThree = 3
  fromEnum DieFour  = 4
  fromEnum DieFive  = 5
  fromEnum DieSix   = 6
  toEnum 1 = DieOne
  toEnum 2 = DieTwo
  toEnum 3 = DieThree
  toEnum 4 = DieFour
  toEnum 5 = DieFive
  toEnum 6 = DieSix
  toEnum _ = error "int too big"

rollDie :: State StdGen Die
rollDie = toEnum <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie = flip replicateM rollDie

repeatM :: Applicative m => m a -> m [a]
repeatM ma = (:) <$> ma <*> repeatM ma

rollsToGetN :: Int -> State StdGen (Int, [Die])
rollsToGetN n = do
  infDieList <- repeatM rollDie
  let infIntList = fmap fromEnum infDieList
      partialSums = scanl1 (+) infIntList
      numDies = 1 + (length $ takeWhile (< n) partialSums)
  return $ (numDies, take numDies infDieList)
