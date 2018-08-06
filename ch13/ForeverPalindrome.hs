import           Control.Monad (forever)
import           System.Exit   (exitSuccess)

-- what do you mean by "it doesn't work" and "so that it works"?
-- how is it supposed to work on sentences differently than ordinary strings?
palindrome :: IO ()
palindrome =
  forever $ do
    line1 <- getLine
    case line1 == reverse line1 of
      True -> putStrLn "It's a palindrome!"
      False -> do
        putStrLn "Nope"
        exitSuccess
