newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
-- getDog p = Dog (dogName p) (address p)
getDog = Dog <$> dogName <*> address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g ax ay = g <$> ax <*> ay

--------------------------------------------------------------------------------

newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure a = Reader $ \x -> a
  (<*>) (Reader f) (Reader ra) = Reader $ \r -> (f r) (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) (Reader ra) f =
    Reader $ \r ->
      let a = ra r
          newReader = f a
          rTob = runReader newReader
       in rTob r

getDogRM :: Reader Person Dog
getDogRM = do
    name <- Reader dogName
    addy <- Reader address
    return $ Dog name addy
