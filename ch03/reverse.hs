module Reverse where

rvrs :: String -> String
rvrs x =
  let noExclamation = init x
      awesome = drop 9 noExclamation
      curry = take 5 noExclamation
      is = drop 6 $ take 8 noExclamation
   in awesome ++ " " ++ is ++ " " ++ curry

main :: IO ()
main = print $ rvrs "Curry is awesome!"
