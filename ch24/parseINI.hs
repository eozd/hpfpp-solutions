{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Ini where

import           Control.Applicative
import qualified Data.ByteString     as BS
import           Data.Char           (isAlpha)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Trifecta

testData :: IO BS.ByteString
testData = BS.readFile "/home/eozd/hpfpp/src/ch24/ex.ini"

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

type Name = String

type Value = String

type Assignments = Map Name Value

data Section =
  Section Header
          Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWS :: Parser ()
skipWS = skipMany (char ' ' <|> char '\n')

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

skipEOL :: Parser ()
skipEOL = skipMany (char '\n')

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  val <- some (notChar '\n')
  skipEOL
  return (name, val)

skipComments :: Parser ()
skipComments =
  skipMany $ do
    char ';' <|> char '#'
    skipMany (notChar '\n')
    skipEOL

parseSection :: Parser Section
parseSection = do
  skipWS
  skipComments
  header <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section header (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section header assignments) m = M.insert header assignments m

parseINI :: Parser Config
parseINI = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return $ Config mapOfSections

main :: IO ()
main = do
  iniStr <- testData
  print $ runParser parseINI mempty iniStr
