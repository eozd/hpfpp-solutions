module GenForDatatypes where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = frequency [(2, return Fulse), (1, return Frue)]
-- genFool = elements [Fulse, Frue]

instance Arbitrary Fool where
    arbitrary = genFool
