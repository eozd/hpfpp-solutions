module Exercises where

res1 = 2 + 2 * 3 - 1 == 2 + (2 * 3) - 1

res2 = ((^) 10 $ 1 + 1) == (^) 10 (1 + 1)

res3 = 2 ^ 2 * 4 ^ 5 + 1 == ((2 ^ 2) * (4 ^ 5)) + 1

res4 = True == (1 + 1 == 2)

res5 = True == (10 ^ 2 == 10 + 9 * 10)

res6 = False == (400 - 37 == (-) 37 400)

res7 = False == ((fromIntegral $ 100 `div` 3) == 100 / 3)

res8 = False == (2 * 5 + 18 == 2 * (5 + 18))

-- REPL version
-- z = 7
-- y = z + 8
-- x = y ^ 2
-- waxOn = x * 5
--
-- src version
waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

triple x = x * 3

waxOff x = triple x
