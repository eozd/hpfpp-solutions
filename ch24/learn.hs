module Learn where

import Control.Applicative (liftA2)
import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

parseAll :: Parser String
parseAll = choice [string "123", string "12", string "1", stop]

str :: String -> Parser String
str = foldr ((liftA2 (:)) . char) (pure "")

testParse :: (Show a) => Parser a -> IO ()
-- testParse p = print $ parseString (p >> eof) mempty "123"
testParse p = print $ parseString p mempty "123"
