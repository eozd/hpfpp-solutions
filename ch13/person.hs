module PersonInteractive where

import           Text.Read (readMaybe)

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter a name: "
  name <- getLine
  putStr "Enter an age: "
  ageStr <- getLine
  case readMaybe ageStr :: Maybe Integer of
    Just age -> do
      case mkPerson name age of
        Left NameEmpty -> putStrLn "Name cannot be empty"
        Left AgeTooLow -> putStrLn "Age must be positive"
        Left (PersonInvalidUnknown errMsg) -> putStrLn errMsg
        Right person@(Person name age) ->
          putStrLn $ "Yay! Successfully got a person: " ++ show person
    Nothing -> putStrLn "Age must be an integer"
