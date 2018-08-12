import           Control.Applicative     ((<|>))
import           Data.Char
import           Text.Parser.Combinators
import           Text.Trifecta

digitList :: [Char]
digitList = ['0' .. '9']

parseDigit :: Parser Char
parseDigit = choice $ fmap char digitList

parseBase10Integer :: Parser Integer
parseBase10Integer = do
  sign <- option 1 (char '-' >> return (-1))
  intStr <- some parseDigit
  return $ sign * read intStr

main :: IO ()
main = do
  print $ runParser parseBase10Integer mempty "-123abc"
  print $ runParser parseBase10Integer mempty "abc"
  print $ runParser parseBase10Integer mempty "2"
