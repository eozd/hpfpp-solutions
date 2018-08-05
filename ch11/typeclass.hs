data Doggy a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge

-- 1. Doggy is a type constructor. It takes a type in order to become a
-- concrete type, such as Doggy Int
--
-- 2. :k Doggy is * -> *
--
-- 3. :k Doggy String is *
--
-- 4. :t Husky 10 is (Num a) => Doggy a
--
-- 5. :t Husky (10 :: Integer) is Doggy Integer
--
-- 6. :t Mastiff "Scooby Doo" is Doggy String
--
-- 7. DogueDeBordeaux is both a type constructor and a data constructor
--
-- 8. :t DogueDeBordeaux is a -> DogueDeBordeaux a
--
-- 9. :t DogueDeBordeaux "doggie" is DogueDeBordeaux String
