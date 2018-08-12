{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta

badFraction = "1/0"

alsoBad = "10"

shouldWork = "1/2"

shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  num <- decimal
  char '/'
  denom <- decimal
  case denom of
    0 -> fail "Denominator cannot be 0"
    _ -> return $ num % denom

parseSingleInt :: Parser Integer
parseSingleInt = do
  num <- integer
  eof
  return num

data Token
  = RationalToken Rational
  | DecimalToken Integer
  deriving (Eq, Show)

parseFracOrDec :: Parser Token
parseFracOrDec =
  (RationalToken <$> try parseFraction) <|> (DecimalToken <$> try decimal)

main :: IO ()
main = do
  print $ parseString parseFracOrDec mempty badFraction
  print $ parseString parseFracOrDec mempty alsoBad
  print $ parseString parseFracOrDec mempty shouldWork
  print $ parseString parseFracOrDec mempty shouldAlsoWork
  print $ parseString parseFracOrDec mempty "1742"
  print $ parseString parseFracOrDec mempty "1742ab"
