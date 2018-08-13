import           Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

rDec :: Num a => Reader a a
rDec = reader $ \a -> a - 1

rShow :: Show a => Reader a String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT $ \a -> do
    putStrLn $ "Hi: " ++ show a
    return (a + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
    putStrLn $ "Hi: " ++ show s
    let newState = s + 1
    return $ (show s, newState)
