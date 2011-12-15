module IDE.Undefineditor.Util.Fork where

import Control.Concurrent
import Control.Exception

fork :: IO () -> IO ThreadId
-- we must use forkOS (as opposed to forkIO) or else things like
-- background file saving will not work. The reason is that the idle
-- loop is handled by gtk instead of by the haskell RTS, and so
-- other lightweight threads cannot wake up unless they are forked on
-- separate operating system threads
fork action = forkOS attempt where
  attempt = do
    e <- try action
    case e of
      Left z -> do
        putStr "thread crashed: "
        print (z :: SomeException)
      Right () -> return ()

