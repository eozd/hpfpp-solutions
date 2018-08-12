module SemVer where

import           Data.List     (find)
import           Data.Maybe    (fromMaybe)

import           Text.Trifecta

data NumberOrString
  = Str String
  | Num Integer
  deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Version = (Major, Minor, Patch)

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer
  { major    :: Major
  , minor    :: Minor
  , patch    :: Patch
  , release  :: Release
  , metadata :: Metadata
  } deriving (Eq, Show)

instance Ord NumberOrString where
  compare (Str _) (Num _) = GT
  compare (Num _) (Str _) = LT
  compare (Str x) (Str y) = x `compare` y
  compare (Num x) (Num y) = x `compare` y

instance Ord SemVer where
  compare (SemVer major1 minor1 patch1 release1 _) (SemVer major2 minor2 patch2 release2 _)
    | major1 /= major2 = major1 `compare` major2
    | minor1 /= minor2 = minor1 `compare` minor2
    | patch1 /= patch2 = patch1 `compare` patch2
    | null release1 || null release2 = release2 `compare` release1 -- reversed
    | otherwise =
      let zippedReleases = zip release1 release2
          firstDiff = find (uncurry (/=)) zippedReleases
       in case firstDiff of
            Nothing       -> length release1 `compare` length release2
            Just (r1, r2) -> r1 `compare` r2

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = choice [Num <$> integer, Str <$> some letter]

parseVersion :: Parser Version
parseVersion = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  return (major, minor, patch)

parseRelease :: Parser [NumberOrString]
parseRelease = do
  char '-'
  head <- parseNumberOrString
  rest <- many (char '.' >> parseNumberOrString)
  return $ head : rest

parseMetadata :: Parser [NumberOrString]
parseMetadata = do
  char '+'
  head <- parseNumberOrString
  rest <- many (char '.' >> parseNumberOrString)
  return $ head : rest

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseVersion
  releaseMaybe <- optional parseRelease
  metadataMaybe <- optional parseMetadata
  let release = fromMaybe [] releaseMaybe
      metadata = fromMaybe [] metadataMaybe
  return $ SemVer major minor patch release metadata

main :: IO ()
main = do
  print $ runParser parseSemVer mempty "2.0.1-x.7.z.92+alpha"
