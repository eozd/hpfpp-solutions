-- 1. no bottom
a = snd (undefined, 1)

-- 2. y construction is infinite loop -- bottom
-- let x = undefined
-- let y = x `seq` 1 in snd (x, y)

-- 3. bottom ; spine contains undefined
c = length $ [1..5] ++ undefined

-- 4. no problem; d == 6
d = length $ [1..5] ++ [undefined]

-- 5. no problem
e = const 1 undefined

-- 6. no problem
f = const 1 (undefined `seq` 1)

-- 7. bottom
g = const undefined 1

-- Make bottom out
x = undefined
y = "blah"

main :: IO ()
main = do
    print (snd (x, x `seq` y))
