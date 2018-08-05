{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

newtype Cows =
  Cows Int
  deriving (Eq, Show)

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (n, str) = n > 100

-- instance TooMany (Int, Int) where
--     tooMany (n, m) = tooMany $ n + m

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (a1, a2) = tooMany $ a1 + a2
