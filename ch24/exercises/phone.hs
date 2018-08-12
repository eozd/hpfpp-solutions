import           Data.Foldable (foldl')
import           Text.Trifecta

type NumberingPlanArea = Int

npaLen :: Int
npaLen = 3

type Exchange = Int

exchangeLen :: Int
exchangeLen = 3

type LineNumber = Int

lineLen :: Int
lineLen = 4

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange
              LineNumber
  deriving (Eq, Show)

parseUsualPhone :: Parser PhoneNumber
parseUsualPhone = do
  npa <- read <$> count npaLen digit
  char '-'
  exchange <- read <$> count exchangeLen digit
  char '-'
  line <- read <$> count lineLen digit
  return $ PhoneNumber npa exchange line

parseCombinedPhone :: Parser PhoneNumber
parseCombinedPhone = do
  num <- count (npaLen + exchangeLen + lineLen) digit
  let npa = read $ take npaLen num
      exchange = read $ (take exchangeLen . drop npaLen) num
      line = read $ drop (npaLen + exchangeLen) num
  return $ PhoneNumber npa exchange line

parseParenthesizedPhone :: Parser PhoneNumber
parseParenthesizedPhone = do
  char '('
  npa <- read <$> count npaLen digit
  string ") "
  exchange <- read <$> count exchangeLen digit
  char '-'
  line <- read <$> count lineLen digit
  return $ PhoneNumber npa exchange line

parseCountryCodedPhone :: Parser PhoneNumber
parseCountryCodedPhone = do
  count 1 digit
  char '-'
  parseUsualPhone

parsePhone :: Parser PhoneNumber
parsePhone =
  choice
    [ try parseUsualPhone
    , try parseCombinedPhone
    , try parseParenthesizedPhone
    , try parseCountryCodedPhone
    ]

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
