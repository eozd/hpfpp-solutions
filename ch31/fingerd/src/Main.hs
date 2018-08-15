{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad                (forever)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BSC
import           Data.List                    (intersperse)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Database.SQLite.Simple       hiding (bind, close)
import qualified Database.SQLite.Simple       as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket               hiding (recv)
import           Network.Socket.ByteString    (recv, sendAll)
import           User
import           UserDB

createDB :: IO ()
createDB = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUserQuery meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where
    meRow :: UserRow
    meRow =
      (Null, "eozd", "/bin/zsh", "/home/eozd", "Esref Ozdemir", "527-746-2257")

formatUser :: User -> ByteString
formatUser user =
  BS.concat
    [ "User ID: "
    , e . T.pack . show $ (userId user)
    , "\n"
    , "Login: "
    , e . username $ user
    , "\t\t\t\t"
    , "Name: "
    , e . realName $ user
    , "\n"
    , "Directory: "
    , e . homeDir $ user
    , "\t\t\t"
    , "Shell: "
    , e . shell $ user
    , "\n"
    ]
  where
    e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn sock uname = do
  maybeUser <- getUser dbConn (T.strip uname)
  case maybeUser of
    Nothing -> do
      let errMsg =
            BS.concat
              ["Couldn't find matching user for username: ", (encodeUtf8 uname)]
      BSC.putStrLn errMsg
      sendAll sock errMsg
    Just user -> sendAll sock (formatUser user)

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn sock = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll sock (encodeUtf8 newlineSeparated)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn sock = do
  msg <- recv sock 1024
  case msg of
    "\r\n" -> returnUsers dbConn sock
    name   -> returnUser dbConn sock (decodeUtf8 name)

handleMultipleQueries :: Connection -> Socket -> IO ()
handleMultipleQueries dbConn sock =
  forever $ do
    (clientSocket, _) <- accept sock
    putStrLn "Got connection, handling query..."
    handleQuery dbConn clientSocket
    close clientSocket

main :: IO ()
main =
  withSocketsDo $ do
    addrInfos <-
      getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing
        (Just "79")
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bind sock (addrAddress serverAddr)
    listen sock 1
    dbConn <- open "finger.db"
    handleMultipleQueries dbConn sock
    SQLite.close dbConn
    close sock
