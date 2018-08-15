{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module UserDB where

import           Control.Exception
import           Data.List                    (isSuffixOf)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Typeable
import           Database.SQLite.Simple       hiding (bind, close)
import           Database.SQLite.Simple.Types
import           Text.RawString.QQ
import           User

createUsers :: Query
createUsers =
  [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT,
   homeDir TEXT,
   realName TEXT,
   phone TEXT)
|]

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

updateUserQuery :: UserDelta -> Query
updateUserQuery user =
  Query $ T.concat ["UPDATE users SET ", T.pack assignments, " WHERE id = :id"]
  where
    assignmentsEndingComma =
      concat
        [ case usernameChange user of
            Nothing -> ""
            Just _  -> "username = :username, "
        , case shellChange user of
            Nothing -> ""
            Just _  -> "shell = :shell, "
        , case homeDirChange user of
            Nothing -> ""
            Just _  -> "homeDir = :homeDir, "
        , case realNameChange user of
            Nothing -> ""
            Just _  -> "realName = :realName, "
        , case phoneChange user of
            Nothing -> ""
            Just _  -> "phone = :phone, "
        ]
    assignments =
      if ", " `isSuffixOf` assignmentsEndingComma
        then take (length assignmentsEndingComma - 2) assignmentsEndingComma
        else assignmentsEndingComma

deleteUserQuery :: Query
deleteUserQuery = "DELETE FROM users WHERE id = ?"

allUsers :: Query
allUsers = "SELECT * FROM users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn uname = do
  results <- query conn getUserQuery (Only uname)
  case results of
    []     -> return $ Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

insertUser :: Connection -> User -> IO ()
insertUser dbConn user = execute dbConn insertUserQuery user

updateUser :: Integer -> UserDelta -> Connection -> IO ()
updateUser uid userDelta dbConn = do
  let queryTemplate = updateUserQuery userDelta
  executeNamed dbConn queryTemplate substitutionList
  where
    substitutionList =
      concat
        [ case usernameChange userDelta of
            Nothing     -> []
            Just change -> [":username" := change]
        , case shellChange userDelta of
            Nothing     -> []
            Just change -> [":shell" := change]
        , case homeDirChange userDelta of
            Nothing     -> []
            Just change -> [":homeDir" := change]
        , case realNameChange userDelta of
            Nothing     -> []
            Just change -> [":realName" := change]
        , case phoneChange userDelta of
            Nothing     -> []
            Just change -> [":phone" := change]
        , [":id" := uid]
        ]

deleteUser :: Connection -> Integer -> IO ()
deleteUser dbConn uid = execute dbConn deleteUserQuery (Only uid)
