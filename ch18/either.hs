module EitherMonad
  ( SoftwareShop
  , mkSoftware
  ) where

type Founded = Int

type Coders = Int

data SoftwareShop = Shop
  { founded     :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded
                          Coders
  deriving (Eq, Show)

validateFounded :: SoftwareShop -> Either FoundedError SoftwareShop
validateFounded shop@(Shop n _)
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right shop

validateCoders :: SoftwareShop -> Either FoundedError SoftwareShop
validateCoders shop@(Shop _ coders)
  | coders < 0 = Left $ NegativeCoders coders
  | coders > 500 = Left $ TooManyCoders coders
  | otherwise = Right shop

validateCodersForYears :: SoftwareShop -> Either FoundedError SoftwareShop
validateCodersForYears shop@(Shop years coders)
  | coders > div years 10 = Left $ TooManyCodersForYears years coders
  | otherwise = Right shop

mkSoftware :: Founded -> Coders -> Either FoundedError SoftwareShop
mkSoftware years coders =
  return (Shop years coders) >>= validateFounded >>= validateCoders >>=
  validateCodersForYears
