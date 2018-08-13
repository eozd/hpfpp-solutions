{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config = Config
  { counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let maybeCount = M.lookup k m
   in case maybeCount of
        Just cnt ->
          let newCnt = cnt + 1
              newMap = M.insert k newCnt m
           in (newMap, newCnt)
        Nothing ->
          let newMap = M.insert k 1 m
           in (newMap, 1)

app :: Scotty ()
app =
    get "/:key" $ do
        unprefixed <- param "key"
        (Config countRef prefix) <- lift ask
        countMap <- liftIO $ readIORef countRef
        let key' = prefix <> unprefixed
            (newMap, newCnt) = bumpBoomp key' countMap
        liftIO (writeIORef countRef newMap)
        html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newCnt, "</h1>"]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter (TL.pack prefixArg)
        runR = \(ReaderT f) -> f config
    scottyT 3000 runR app
