{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Char                  (isDigit)
import           Data.Text                  (pack)
import           Database.SQLite.Simple     hiding (bind, close)
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           System.IO                  (BufferMode (NoBuffering),
                                             hSetBuffering, stdout)
import           Text.RawString.QQ
import           Text.Read                  (readMaybe)
import           User
import           UserDB

data Action
  = Insert
  | Update
  | Delete

data Mode
  = Interactive
  | FromArgs

validateHomeDir :: String -> ExceptT String IO ()
validateHomeDir homeDir =
  if ' ' `elem` homeDir
    then ExceptT $ return (Left "Home directory cannot contain spaces")
    else ExceptT $ return (Right ())

validatePhone :: String -> ExceptT String IO ()
validatePhone phone =
  let numHyphens = length $ filter (== '-') phone
   in if numHyphens == 2
        then ExceptT $ return (Right ())
        else ExceptT $ return (Left "Phone must contain exactly 2 hyphens (-)")

validateRealName :: String -> ExceptT String IO ()
validateRealName realName =
  if (length $ filter isDigit realName) > 0
    then ExceptT $ return (Left "Real name cannot contain digits")
    else ExceptT $ return (Right ())

validateUIDStr :: String -> ExceptT String IO Integer
validateUIDStr uidStr =
  let maybeUID = readMaybe uidStr :: Maybe Integer
   in case maybeUID of
        Nothing  -> ExceptT $ return (Left "User ID must be integer")
        Just uid -> ExceptT $ return (Right uid)

validateUsername :: String -> ExceptT String IO ()
validateUsername username =
  if ' ' `elem` username
    then ExceptT $ return (Left "Username must be a single word")
    else ExceptT $ return (Right ())

validateShell :: String -> ExceptT String IO ()
validateShell shell =
  if ' ' `elem` shell
    then ExceptT $ return (Left "Shell must be a single word")
    else ExceptT $ return (Right ())

getUserInteractive :: ExceptT String IO User
getUserInteractive = do
  liftIO $ hSetBuffering stdout NoBuffering
  ---
  liftIO $ putStr "User ID: "
  uidStr <- liftIO getLine
  uid <- validateUIDStr uidStr
  ---
  liftIO $ putStr "Username: "
  username <- liftIO getLine
  validateUsername username
  ---
  liftIO $ putStr "Shell: "
  shell <- liftIO getLine
  validateShell shell
  ---
  liftIO $ putStr "Home directory: "
  homeDir <- liftIO getLine
  validateHomeDir homeDir
  ---
  liftIO $ putStr "Real name: "
  realName <- liftIO getLine
  validateRealName realName
  ---
  liftIO $ putStr "Phone: "
  phone <- liftIO getLine
  validatePhone phone
  ---
  return $
    User
      uid
      (pack username)
      (pack shell)
      (pack homeDir)
      (pack realName)
      (pack phone)

getUserFromArgs :: [String] -> ExceptT String IO User
getUserFromArgs args = do
  let [uidStr, username, shell, homeDir, realName, phone] = args
  uid <- validateUIDStr uidStr
  validateUsername username
  validateShell shell
  validateHomeDir homeDir
  validateRealName realName
  validatePhone phone
  return $
    User
      uid
      (pack username)
      (pack shell)
      (pack homeDir)
      (pack realName)
      (pack phone)

getDeleteIdFromArgs :: [String] -> ExceptT String IO Integer
getDeleteIdFromArgs args = do
  let [uidStr] = args
  uid <- validateUIDStr uidStr
  return uid

getDeleteIdInteractive :: ExceptT String IO Integer
getDeleteIdInteractive = do
  liftIO $ hSetBuffering stdout NoBuffering
  liftIO $ putStr "User ID: "
  uidStr <- liftIO getLine
  uid <- validateUIDStr uidStr
  return uid

noUpdateStr :: String
noUpdateStr = "_"

type ValidationFunction = String -> ExceptT String IO ()

getDelta :: String -> ValidationFunction -> ExceptT String IO (Maybe String)
getDelta str validationF = do
  if str == noUpdateStr
    then return Nothing
    else do
      validationF str
      return $ Just str

getUpdateUserInteractive :: ExceptT String IO (Integer, UserDelta)
getUpdateUserInteractive = do
  liftIO $ hSetBuffering stdout NoBuffering
  ---
  liftIO $ putStr "User ID: "
  uidStr <- liftIO getLine
  uid <- validateUIDStr uidStr
  ---
  liftIO $ putStr "Username: "
  username <- liftIO getLine
  usernameDelta <- getDelta username validateUsername
  ---
  liftIO $ putStr "Shell: "
  shell <- liftIO getLine
  shellDelta <- getDelta shell validateShell
  ---
  liftIO $ putStr "Home directory: "
  homeDir <- liftIO getLine
  homeDirDelta <- getDelta homeDir validateHomeDir
  ---
  liftIO $ putStr "Real name: "
  realName <- liftIO getLine
  realNameDelta <- getDelta realName validateRealName
  ---
  liftIO $ putStr "Phone: "
  phone <- liftIO getLine
  phoneDelta <- getDelta phone validatePhone
  ---
  return $
    ( uid
    , UserDelta
        (pack <$> usernameDelta)
        (pack <$> shellDelta)
        (pack <$> homeDirDelta)
        (pack <$> realNameDelta)
        (pack <$> phoneDelta))

getUpdateUserFromArgs :: [String] -> ExceptT String IO (Integer, UserDelta)
getUpdateUserFromArgs args = do
  let [uidStr, username, shell, homeDir, realName, phone] = args
  uid <- validateUIDStr uidStr
  usernameDelta <- getDelta username validateUsername
  shellDelta <- getDelta shell validateShell
  homeDirDelta <- getDelta homeDir validateHomeDir
  realNameDelta <- getDelta realName validateRealName
  phoneDelta <- getDelta phone validatePhone
  return $
    ( uid
    , UserDelta
        (pack <$> usernameDelta)
        (pack <$> shellDelta)
        (pack <$> homeDirDelta)
        (pack <$> realNameDelta)
        (pack <$> phoneDelta))

helpMsg :: String
helpMsg =
  [r|
Usage: insert_user insert (-i | id username shell home realname phone)
       insert_user update (-i | id new_username new_shell new_home new_realname new_phone)
       insert_user delete (-i | id)

When updating, non-changing fields can be given as _

OPTIONS:
    -i          Interactive mode
    id          User database ID (primary key)
    username    Username
    shell       User shell
    home        User home directory
    realname    User's real name
    phone       User's phone|]

validateArgs :: [String] -> ExceptT String IO (Action, Mode)
validateArgs [] = ExceptT $ return (Left helpMsg)
validateArgs args =
  let action = head args
   in case action of
        "insert" ->
          if length args == 2 && last args == "-i"
            then ExceptT $ return (Right (Insert, Interactive))
            else if length args == 7
                   then ExceptT $ return (Right (Insert, FromArgs))
                   else ExceptT $ return (Left helpMsg)
        "update" ->
          if length args == 2 && last args == "-i"
            then ExceptT $ return (Right (Update, Interactive))
            else if length args == 7
                   then ExceptT $ return (Right (Update, FromArgs))
                   else ExceptT $ return (Left helpMsg)
        "delete" ->
          if length args == 2
            then if last args == "-i"
                   then ExceptT $ return (Right (Delete, Interactive))
                   else ExceptT $ return (Right (Delete, FromArgs))
            else ExceptT $ return (Left helpMsg)
        _ -> ExceptT $ return (Left helpMsg)

main :: IO ()
main = do
  args <- getArgs
  eitherFunc <-
    runExceptT $ do
      (action, mode) <- validateArgs args
      let argsWithoutAction = tail args
      case action of
        Insert ->
          case mode of
            Interactive -> do
              user <- getUserInteractive
              return $ (flip insertUser) user
            FromArgs -> do
              user <- getUserFromArgs argsWithoutAction
              return $ (flip insertUser) user
        Update ->
          case mode of
            Interactive -> do
              (uid, userDelta) <- getUpdateUserInteractive
              return $ updateUser uid userDelta
            FromArgs -> do
              (uid, userDelta) <- getUpdateUserFromArgs argsWithoutAction
              return $ updateUser uid userDelta
        Delete ->
          case mode of
            Interactive -> do
              uid <- getDeleteIdInteractive
              return $ (flip deleteUser) uid
            FromArgs -> do
              uid <- getDeleteIdFromArgs argsWithoutAction
              return $ (flip deleteUser) uid
  case eitherFunc of
    Left err -> putStrLn err >> exitFailure
    Right f -> do
      dbConn <- open "finger.db"
      f dbConn
