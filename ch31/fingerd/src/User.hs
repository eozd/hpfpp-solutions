module User where

import           Data.Text              (Text)
import           Database.SQLite.Simple hiding (bind, close)

data User = User
  { userId   :: Integer
  , username :: Text
  , shell    :: Text
  , homeDir  :: Text
  , realName :: Text
  , phone    :: Text
  } deriving (Eq, Show)

data UserDelta = UserDelta
  { usernameChange :: Maybe Text
  , shellChange    :: Maybe Text
  , homeDirChange  :: Maybe Text
  , realNameChange :: Maybe Text
  , phoneChange    :: Maybe Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow user =
    toRow
      ( userId user
      , username user
      , shell user
      , homeDir user
      , realName user
      , phone user)
