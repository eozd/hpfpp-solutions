{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Applicative
import           Control.Monad              (foldM)
import           Control.Monad.Trans.Except
import qualified Data.ByteString            as BS
import           Data.Char                  (isAlpha)
import           Data.List                  (isSuffixOf)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, readFile, stderr)
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Trifecta

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

validateArgs :: [String] -> ExceptT String IO [String]
validateArgs [dir] = ExceptT $ return (Right [dir])
validateArgs _ =
  ExceptT $ do
    progName <- getProgName
    return (Left $ "Usage: " ++ progName ++ " <directory>")

validateDir :: FilePath -> ExceptT String IO ()
validateDir dir =
  ExceptT $ do
    dirExists <- doesDirectoryExist dir
    if dirExists
      then return (Right ())
      else return (Left $ "Given directory " ++ dir ++ " doesn't exist!")

main :: IO ()
main = do
  args <- getArgs
  dir <-
    runExceptT $ do
      [dir] <- validateArgs args
      validateDir dir
      return dir
  case dir of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right dir -> do
      fileList <- listDirectory dir
      let iniFiles = filter (".ini" `isSuffixOf`) fileList
          iniPaths = map ((dir ++ "/") ++) iniFiles
          parseMap = M.empty
      finalMap <-
        foldM
          (\m path -> do
             contents <- readFile path
             let parseRes = parseString parseINI mempty contents
             case parseRes of
               Success cfg -> return $ M.insert path cfg m
               Failure err -> do
                 hPutStrLn stderr (show err)
                 exitFailure)
          M.empty
          iniPaths
      print finalMap
