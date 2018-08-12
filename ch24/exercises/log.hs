{-# LANGUAGE QuasiQuotes #-}

module LogParsing where

import           Control.Applicative ((<|>))
import           Data.Char
import           Data.List           (intercalate)
import qualified Test.QuickCheck     as QC
import           Text.Printf         (printf)
import           Text.RawString.QQ
import           Text.Trifecta

--------------------------------- TYPE ALIASES ---------------------------------
type Year = Int

type Month = Int

type Day = Int

type Hour = Int

type Minute = Int

type Description = String

--------------------------------- TYPES ----------------------------------------
data Date = Date
  { year  :: Year
  , month :: Month
  , day   :: Day
  } deriving (Eq)

data Time = Time
  { hour   :: Hour
  , minute :: Minute
  } deriving (Eq)

newtype Header =
  Header Date
  deriving (Eq)

data Activity = Activity
  { time  :: Time
  , descr :: Description
  } deriving (Eq)

data Section =
  Section Header
          [Activity]
  deriving (Eq)

data LogFile =
  LogFile [Section]
  deriving (Eq)

--------------------------------- SHOW -----------------------------------------
instance Show Time where
  show (Time h m) = printf "%02d" h ++ ":" ++ printf "%02d" m

instance Show Date where
  show (Date y m d) = show y ++ "-" ++ printf "%02d" m ++ "-" ++ printf "%02d" d

instance Show Header where
  show (Header date) = "# " ++ show date ++ "\n"

instance Show Activity where
  show (Activity time descr) = show time ++ " " ++ descr ++ "\n"

instance Show Section where
  show (Section header actList) = show header ++ (concatMap show actList)

instance Show LogFile where
  show (LogFile secList) = intercalate "\n" . map show $ secList

--------------------------- PARSING FUNCTIONS ----------------------------------
skipEOL :: Parser ()
skipEOL = choice [eof, (string "\n" <|> string "\r\n") >> return ()]

skipComment :: Parser ()
skipComment = string "--" >> many (noneOf "\n\r") >> skipEOL

inlineWhiteSpace :: Parser ()
inlineWhiteSpace =
  skipMany $ satisfy (\c -> c /= '\n' && c /= '\r' && isSpace c)

parseDate :: Parser Date
parseDate = do
  year <- fromInteger <$> integer
  char '-'
  month <- fromInteger <$> integer
  char '-'
  day <- fromInteger <$> integer
  return $ Date year month day

parseTime :: Parser Time
parseTime = do
  hour <- read <$> count 2 digit
  char ':'
  minute <- read <$> count 2 digit
  return $ Time hour minute

parseHeader :: Parser Header
parseHeader = do
  char '#'
  optional inlineWhiteSpace
  date <- parseDate
  optional inlineWhiteSpace
  optional skipComment
  return $ Header date

parseActivity :: Parser Activity
parseActivity = do
  time <- parseTime
  inlineWhiteSpace
  descr <-
    manyTill
      anyChar
      (try (inlineWhiteSpace >> skipEOL) <|>
       try (inlineWhiteSpace >> skipComment))
  return $ Activity time descr

parseSection :: Parser Section
parseSection = do
  header <- parseHeader
  activityList <- many parseActivity
  return $ Section header activityList

parseLogFile :: Parser LogFile
parseLogFile = do
  skipMany (skipComment <|> skipWS)
  secList <-
    many $ do
      section <- parseSection
      skipMany (skipComment <|> skipWS)
      return section
  return $ LogFile secList
  where
    skipWS = skipSome (oneOf "\n\r\t ")

------------------------------- TEST DATA --------------------------------------
exampleSection :: String
exampleSection =
  [r|# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep|]

exampleLogFile :: String
exampleLogFile =
  [r|

  -- whee a comment

  -- whee a comment
  --

# 2025-02-05 -- such a nice date
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower  -- another comment
21:15 Read  -- commmeeeent
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep

-- some more comments
|]

------------------------------ ARBITRARY ---------------------------------------
-- Generate arbitrary and properly structured LogFile types.

instance QC.Arbitrary Section where
  arbitrary =
    Section <$> QC.arbitrary <*>
    QC.arbitrary `QC.suchThat`
    (\actList -> (not . null) actList && length actList < 15)

instance QC.Arbitrary Header where
  arbitrary = Header <$> QC.arbitrary

instance QC.Arbitrary Date where
  arbitrary =
    Date <$> QC.arbitrary `QC.suchThat` (\year -> year > 0) <*>
    QC.arbitrary `QC.suchThat` (\month -> month > 0 && month <= 12) <*>
    QC.arbitrary `QC.suchThat` (\day -> day > 0 && day <= 30)

instance QC.Arbitrary Time where
  arbitrary =
    Time <$> QC.arbitrary `QC.suchThat` (\hour -> hour >= 0 && hour < 24) <*>
    QC.arbitrary `QC.suchThat` (\minute -> minute >= 0 && minute < 60)

instance QC.Arbitrary Activity where
  arbitrary =
    Activity <$> QC.arbitrary <*>
    QC.arbitrary `QC.suchThat`
    (\descr ->
       (not . null) descr &&
       length descr < 30 &&
       all (\c -> c == ' ' || isAlphaNum c) descr &&
       isAlphaNum (last descr) && isAlphaNum (head descr))

instance QC.Arbitrary LogFile where
  arbitrary =
    LogFile <$> QC.arbitrary `QC.suchThat` (\secList -> length secList < 10)

parseShowIdentity :: QC.Property
parseShowIdentity =
  QC.forAll
    (QC.arbitrary :: QC.Gen LogFile)
    (\file ->
       case parseString parseLogFile mempty (show file) of
         Success parsedFile -> file == parsedFile
         Failure _          -> False)

------------------------------- TESTING ----------------------------------------
main :: IO ()
main
  -- print $ parseString parseActivity mempty "06:02 Sanitizing moisture collector -- comment"
  -- print $ parseString skipComment mempty "-- this is a comment"
  -- print $ parseString (whiteSpace >> string "abc") mempty "\n   \nabc"
  -- print $ parseString parseDate mempty "2025-02-05"
  -- print $ parseString parseDate mempty "207-12-5"
  -- print $ parseString parseHeader mempty "# 2025-02-05 -- header comment  "
  -- print $ parseString parseTime mempty "08:00"
  -- print $ parseString parseTime mempty "06:02"
  -- print $ parseString parseActivity mempty "06:02 Sanitizing moisture collector"
  -- print $ parseString parseSection mempty exampleSection
  -- print $ parseString parseLogFile mempty exampleLogFile
 = do
  QC.quickCheck parseShowIdentity
