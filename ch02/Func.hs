sayHello :: String -> IO ()
sayHello name = putStrLn $ "Hello " ++ name

circleArea :: (Floating a) => a -> a
circleArea radius = pi * (radius * radius)
