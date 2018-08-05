import Data.Int

-- 1. Big True
--    Big False
--    Small True
--    Small False
--    -----------
--    cardinality = 4
data BigSmall = Big Bool | Small Bool

-- 2. Numba can take 2^8 values.
--    Boolybool can take 2 values.
--
--    Cardinality of NumberOrBool is (2^8 + 2)
data NumberOrBool = Numba Int8 | Boolybool Bool
