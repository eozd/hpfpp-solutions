module Exercises where

-- Exercise 1
a1 = concat [[1, 2, 3], [4, 5, 6]]

b1 = (++) [1, 2, 3] [4, 5, 6]

c1 = (++) "hello" " world"

d1 = ["hello" ++ " world"]

e1 = "hello" !! 4 -- (!!) "hello" 4

f1 = (!!) "hello" 4

g1 = take 4 "lovely"

h1 = take 3 "awesome"

-- Exercise 2
a2 = (concat [[1 * 6], [2 * 6], [3 * 6]]) == [6, 12, 18]

b2 = ("rain" ++ drop 2 "elbow") == "rainbow"

c2 = (10 * head [1, 2, 3]) == 10

d2 = ((take 3 "Julie") ++ (tail "yes")) == "Jules"

e2 =
  (concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]) ==
  [2, 3, 5, 6, 8, 9]

-- Exercise 3
a3 :: [a] -> [a]
a3 list = init list

b3 :: [a] -> [a]
b3 list = [head $ drop 4 list]

c3 :: [a] -> [a]
c3 list = drop 9 list

thirdLetter :: String -> Char
thirdLetter x = x !! 2

globStr :: String
globStr = "Curry is awesome!"

letterIndex :: Int -> Char
letterIndex n = globStr !! n

rvrs :: String -> String
rvrs x =
  let noExclamation = init x
      awesome = drop 9 noExclamation
      curry = take 5 noExclamation
      is = drop 6 $ take 8 noExclamation
   in awesome ++ " " ++ is ++ " " ++ curry
