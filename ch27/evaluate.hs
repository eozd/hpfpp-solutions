-- (\1 -> \_ -> 1) == 1
a = const 1 undefined

-- (\undefined -> \_ -> undefined) == undefined
b = const undefined 1

-- (\1 -> \_ -> 1) == 1
c = flip const undefined 1

-- (\undefined -> \_ -> undefined) == undefined
d = flip const 1 undefined

-- (\undefined -> \_ -> undefined) == undefined
e = const undefined undefined

-- ('a' `const` ('b' `const` ('c' `const` ('d' `const` ('e' `const` z))))) == 'a'
f = foldr const 'z' ['a'..'e']

-- ('a' `flip const` ('b' `flip const` ('c' `flip const` ('d' `flip const` ('e' `flip const` z))))) == 'z'
g = foldr (flip const) 'z' ['a'..'e']
