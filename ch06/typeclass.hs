module Days where

data DayOfWeek
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Ord, Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

data Date =
  Date DayOfWeek
       Int
  deriving (Show)

instance Eq Date where
  (==) (Date w1 d1) (Date w2 d2) = (w1 == w2) && (d1 == d2)
