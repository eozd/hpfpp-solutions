module Cow
  ( Cow
  , mkCow
  ) where

import           Control.Monad

data Cow = Cow
  { name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

nameCheck :: Cow -> Maybe Cow
nameCheck cow@(Cow "" _ _) = Nothing
nameCheck cow              = Just cow

ageCheck :: Cow -> Maybe Cow
ageCheck cow@(Cow _ age _)
  | age < 0 = Nothing
  | otherwise = Just cow

weightCheck :: Cow -> Maybe Cow
weightCheck cow@(Cow name _ weight) =
  if weight < 0 || name == "Bess" && weight > 499
    then Nothing
    else Just cow

mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name age weight =
  return (Cow name age weight) >>= nameCheck >>= ageCheck >>= weightCheck
