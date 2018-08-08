data Cow = Cow
  { name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s  = Just s

noNegative :: Int -> Maybe Int
noNegative x =
  if x < 0
    then Nothing
    else Just x

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
  Cow <$> noEmpty name <*> noNegative age <*> noNegative weight
