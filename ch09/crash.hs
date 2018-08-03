-- crashes when evaluating 2^undefined
a1 = [x^y | x <- [1..5], y <- [2, undefined]]

-- works; only 1^2 is evaluated
b1 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

-- crash
c1 = sum [1, undefined, 3]

-- works; values don't matter when measuring the spine
d1 = length [1, 2, undefined]

-- crash; Cons itself is undefined
e1 = length $ [1, 2, 3] ++ undefined

-- works; evaluates 2 only
f1 = take 1 $ filter even [1, 2, 3, undefined]

-- crash; evaluates undefined
g1 = take 1 $ filter even [1, 3, undefined]

-- works
h1 = take 1 $ filter odd [1, 3, undefined]

-- works
i1 = take 2 $ filter odd [1, 3, undefined]

-- crash
j1 = take 3 $ filter odd [1, 3, undefined]

-- works; only the first two values are evaluated
k1 = take 2 $ map (+1) [1, 2, undefined]

myFilter = filter (not . (`elem` ["the", "a", "and"])) . words
