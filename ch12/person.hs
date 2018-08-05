type Name = String
type Age = Integer

data Person = Person Name Age deriving (Eq, Show)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | (not . null) name && age >= 0 = Just $ Person name age
  | otherwise = Nothing

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkPersonEither :: Name -> Age -> Either PersonInvalid Person
mkPersonEither name age
  | null name = Left NameEmpty
  | age < 0 = Left AgeTooLow
  | otherwise = Right $ Person name age

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
  | age < 0 = Left [AgeTooLow]
  | otherwise = Right age

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name
  | null name = Left [NameEmpty]
  | otherwise = Right name

mkPersonValidate :: Name -> Age -> ValidatePerson Person
mkPersonValidate name age = mkPersonValidate' (nameOkay name) (ageOkay age)

mkPersonValidate' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPersonValidate' (Left xs) (Left ys) = Left $ xs ++ ys
mkPersonValidate' (Left xs) _ = Left xs
mkPersonValidate' _ (Left ys) = Left ys
mkPersonValidate' (Right name) (Right age) = Right $ Person name age
