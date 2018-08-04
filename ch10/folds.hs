-- 1.
-- Since (*) operation is commutative, the result is irrelevant of which fold
-- type we use
a1 = foldr (*) 1 [1..5]
b1 = foldl (flip (*)) 1 [1..5]
c1 = foldl (*) 1 [1..5]

-- 2. 
a2 = foldl (flip (*)) 1 [1..3]

-- Evaluation Steps
-- ----------------
-- (3 * (2 * (1 * 1)))

-- 3.
-- a) No, both foldr and foldl traverse the spine from left to right due to the
-- way lists are defined
--
-- b) No, if the function passed to foldr doesn't evaluate its second parameter,
-- then the rest of the fold won't be evaluated, as well
--
-- c) Yes, foldr associates to the right. (foldl associates to the left)
--
-- d) No, both foldr and foldl are recursive catamorphisms.

-- 4.
-- Folds are catamorphisms which means they are used to reduce structures to
-- obtain a new value which itself may be a new structure

-- 5.
a5 = foldr (++) "" ["woot", "WOOT", "woot"]
b5 = foldr max (minBound :: Char) "fear is the little death"
c5 = foldr (&&) True [False, True]
d5 = foldr (||) False [False, True]
e5 = foldl (\acc x -> acc ++ show x) "" [1..5]
f5 = foldr (flip const) 'a' [1..5]
g5 = foldr (flip const) 0 "tacos"
h5 = foldl const 0 "burritos"
i5 = foldl const 'z' [1..5]

-- foldl unconditionally evaluates the spine. This is an important distinction
-- from foldr.
--
-- With foldr, the recursive foldr evaluation becomes the second
-- argument of the passed function. Therefore, if the passed function doesn't
-- force the evaluation of its second argument, the spine is not traversed.
--
-- With foldl, the recursive foldl evaluation becomes the argument of the foldl
-- call itself. Therefore, spine is always traversed. Additionally, before
-- evaluating any function calls, foldl produces unevaluated expressions for the
-- whole list. Therefore, it is inefficient to use for large lists. Use foldl'.
