import Data.List
import Data.Char

type Digit = Char
type Letter = Char
type Presses = Int
type Cell = (Digit, [Letter])
data Phone = Phone { cells :: [Cell] }

ourPhone :: Phone
ourPhone = Phone
    [
        ('1', "1"),
        ('2', "abc2"),
        ('3', "def3"),
        ('4', "ghi4"),
        ('5', "jkl5"),
        ('6', "mno6"),
        ('7', "pqrs7"),
        ('8', "tuv8"),
        ('9', "wxyz9"),
        ('*', ""),
        ('0', "+ 0"),
        ('#', ".,")
    ]

convo :: [String]
convo =
    [
        "Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"
    ]

-- Error prone solution
-- 
-- reverseTaps :: Phone -> Char -> [(Digit, Presses)]
-- reverseTaps phone char =
--     let lowerChar = toLower char
--         maybeContainingCell = find (\(dig, letters) -> lowerChar `elem` letters) $ cells phone
--         initRes = if isUpper char then [('*', 1)] else []
--     in case maybeContainingCell of
--          Just (dig, letters) -> case elemIndex lowerChar letters of
--                                   Just i -> initRes ++ [(dig, i + 1)]
--                                   Nothing -> error $ "not even possible"
--          Nothing -> error $ "incorrect char: " ++ show char
--
-- cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
-- cellPhonesDead phone str = concat $ map (reverseTaps phone) str

-- Monadic Maybe solution
--
reverseTaps :: Phone -> Char -> Maybe [(Digit, Presses)]
reverseTaps phone char =
    let lowerChar = toLower char
        initRes = if isUpper char then [('*', 1)] else []
        maybeContainingCell = find
            (\(dig, letters) -> lowerChar `elem` letters)
            (cells phone)
    in do
        (dig, letters) <- maybeContainingCell
        i <- elemIndex lowerChar letters
        return $ initRes ++ [(dig, i + 1)]

cellPhonesDead :: Phone -> String -> Maybe [(Digit, Presses)]
cellPhonesDead phone str = fmap concat $ mapM (reverseTaps phone) str

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- ex: fmap fingerTaps $ cellPhonesDead ourPhone "hello world"
-- --  Just 26
