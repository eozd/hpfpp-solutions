data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | Catapult
  | Take
  deriving (Eq, Show)

data Size =
  Double Double
         Double
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer
        Price
  | Plane Airline
          Size
  deriving (Eq, Show)

mCar = Car Mini (Price 14000)

uCar = Car Mazda (Price 20000)

cCar = Car Tata (Price 7000)

doge = Plane PapuAir

-- 1. :t mCar is Vehicle
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
-- 4. getManu doge should crash
