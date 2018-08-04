module Database where

import           Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1955 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr
    (\x acc ->
       case x of
         (DbDate utcTime) -> utcTime : acc
         otherwise        -> acc)
    []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  foldr
    (\x acc ->
       case x of
         (DbNumber num) -> num : acc
         otherwise      -> acc)
    []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db =
  let numsInDb = filterDbNumber db
   in (fromIntegral $ sum numsInDb) / (fromIntegral $ length numsInDb)
