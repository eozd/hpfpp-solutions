data DivResult
  = Result Integer
  | DivByZero
  deriving (Eq, Show)

div' :: Integer -> Integer -> DivResult
div' _ 0 = DivByZero
div' dividend divisor =
  let finalSign = signum dividend * signum divisor
      absDividend = abs dividend
      absDivisor = abs divisor
   in Result $ finalSign * (go absDividend absDivisor 0)
  where
    go x y count
      | x < y = count
      | otherwise = go (x - y) y (count + 1)
