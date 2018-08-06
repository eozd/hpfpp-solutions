module Main where

import           Data.Char
import           Data.List       (sort)

-- import Test.Hspec
-- import WordNumber (digitToWord, digits, wordNumber)
import           Test.QuickCheck

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, _)       = (Just y, x >= y)

half :: Fractional a => a -> a
half x = x / 2

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_listOrdered :: Property
prop_listOrdered =
  forAll (arbitrary :: Gen [Integer]) (\xs -> listOrdered $ sort xs)

prop_halfIdentity :: Property
prop_halfIdentity =
  forAll (arbitrary :: Gen Double) (\c -> ((* 2) . half) c == c)

prop_plusAssociative :: Property
prop_plusAssociative =
  forAll
    (arbitrary :: Gen (Integer, Integer, Integer))
    (\(x, y, z) -> plusAssociative x y z)

prop_plusCommutative :: Property
prop_plusCommutative =
  forAll (arbitrary :: Gen (Integer, Integer)) (\(x, y) -> plusCommutative x y)

genNumDenom :: Gen (Integer, Integer)
genNumDenom = do
  x <- arbitrary
  y <- arbitrary `suchThat` (/= 0)
  return (x, y)

prop_quotRem :: Property
prop_quotRem = forAll genNumDenom (\(x, y) -> (quot x y) * y + (rem x y) == x)

prop_divMod :: Property
prop_divMod = forAll genNumDenom (\(x, y) -> (div x y) * y + (mod x y) == x)

prop_readShow :: Property
prop_readShow = forAll (arbitrary :: Gen Float) (\x -> x == (read . show $ x))

prop_squareIdentity :: Property
prop_squareIdentity =
  forAll (arbitrary :: Gen Double) (\x -> x == ((^ (2 :: Int)) . sqrt $ x))

twice :: (b -> b) -> (b -> b)
twice f = f . f

fourTimes :: (b -> b) -> (b -> b)
fourTimes = twice . twice

capitalize :: [Char] -> [Char]
capitalize = map toUpper

prop_capitalize :: Property
prop_capitalize =
  forAll
    (arbitrary :: Gen String)
    (\s ->
       (capitalize s == twice capitalize s) &&
       (capitalize s == fourTimes capitalize s))

prop_sort :: Property
prop_sort =
  forAll
    (arbitrary :: Gen [Integer])
    (\xs -> (sort xs == twice sort xs) && (sort xs == fourTimes sort xs))

main :: IO ()
main
    -- hspec $ do
    --     describe "digitToWord" $ do
    --         it "returns zero for 0" $ do
    --             digitToWord 0 `shouldBe` "zero"
    --         it "returns one for 1" $ do
    --             digitToWord 1 `shouldBe` "one"
    --     describe "digits" $ do
    --         it "returns [1] for 1" $ do
    --             digits 1 `shouldBe` [1]
    --         it "returns [1, 0, 0] for 100" $ do
    --             digits 100 `shouldBe` [1, 0, 0]
    --     describe "wordNumber" $ do
    --         it "one-zero-zero given 100" $ do
    --             wordNumber 100 `shouldBe` "one-zero-zero"
    --         it "nine-zero-zero-one for 9001" $ do
    --             wordNumber 9001 `shouldBe` "nine-zero-zero-one"
 = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck prop_readShow
  -- quickCheck prop_squareIdentity
  quickCheck prop_capitalize
  quickCheck prop_sort
