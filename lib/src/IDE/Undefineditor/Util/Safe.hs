-- | Wrapper for actions that should be considered safe.
module IDE.Undefineditor.Util.Safe (safe) where

import Control.Exception (SomeException(), finally, try)
import System.Exit (ExitCode(ExitFailure), exitWith)

-- | Runs the given action, which should raise no exceptions. If an exception is raised,
-- the exception is printed and the application is terminated.
--
-- i.e. this is a tool for finding bugs quickly
safe :: IO () -> IO ()
safe op = do
  x <- try op
  case x of
    Left e -> lastWords $ do
      putStr "Exception in event handler: "
      print (e :: SomeException)
    Right () -> return ()

lastWords action = action `finally` exitWith (ExitFailure 1)
