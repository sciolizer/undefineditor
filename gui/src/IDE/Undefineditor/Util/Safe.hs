module IDE.Undefineditor.Util.Safe (safe) where

import Control.Exception
import System.Exit

safe :: IO () -> IO ()
safe op = do
  x <- try op
  case x of
    Left e -> lastWords $ do
      putStr "Exception in event handler: "
      print (e :: SomeException)
    Right () -> return ()

lastWords action = action `finally` exitWith (ExitFailure 1)
