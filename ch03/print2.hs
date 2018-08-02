module Print2 where

main :: IO ()
main = do
    putStrLn "one"
    putStr "two, "
    putStr "three, "
    putStrLn "four"
