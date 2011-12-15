{-# LANGUAGE
 DeriveDataTypeable
 #-}

-- | A threadpool with one thread. It is intended for jobs which are likely to be invalidated, such
-- as when the job needs to be restarted.
--
-- It can have up to one running action, and up to one queued action. If multiple actions are queued,
-- then all but the most recent one are discarded. A queued action will wait for the currently
-- running action to finish, or for the currently running action to call 'yield'.
module IDE.Undefineditor.Gui.Concurrent.Background (
  Background(),

  newBackground,
  launchBackground,
  yield

) where

import Prelude hiding (catch)

import Control.Concurrent (MVar(), ThreadId(), forkIO, forkOS, modifyMVar, newMVar, readMVar)
import Control.Exception (Exception(), catch, finally, fromException, throwIO)
import Control.Monad (join, void)
import Data.Typeable (Typeable())

data State =
    Idle
  | Running
  | Queued (IO ())

-- | The threadpool
data Background = Background (MVar State) (IO () -> IO ThreadId)

-- | Creates a new threadpool.
newBackground
  :: (IO () -> IO ThreadId) -- ^ 'forkIO' or 'forkOS'. For a gtk application such as this one, you should almost always used 'forkOS'. (The gtk main event loop runs in a foreign-function call, which is inaccessible to the haskell RTS, and so background threads will not run while the gui is idle if you use forkIO.)
  -> IO Background
newBackground fork = do
  return (forkIO, forkOS) -- this line makes the GHC unused import warning go away; import is used by haddock
  mvar <- newMVar Idle
  return (Background mvar fork)

-- | Launches the given action on the given threadpool, or queues the action if the threadpool
-- is busy. Returns immediately.
launchBackground :: Background -> IO () -> IO ()
launchBackground (Background mvar fork) action = join (modifyMVar mvar enqueue) where
  enqueue x =
    case x of
      Idle -> return (Running, void . fork . safeRun $ action)
      Running -> return (Queued action, return ())
      Queued _ -> return (Queued action, return ()) -- new queued action supercedes old queued action
  dequeue :: State -> IO (State, IO ())
  dequeue x =
    case x of
      Idle -> return (Idle, internalBug "state should not be idle while process is running")
      Running -> return (Idle, return ())
      Queued act -> return (Running, safeRun act)
  handler e =
    case fromException e of
      Nothing -> print e
      Just GracefulExit -> return ()
  startNext = join (modifyMVar mvar dequeue)
  safeRun :: IO () -> IO ()
  safeRun act = (act `catch` handler) `finally` startNext

-- | If another action is queued up, terminates the current action. Otherwise does nothing.
--
-- The behavior of this function is undefined if the wrong 'Background' instance is given.
yield :: Background -> IO ()
yield (Background mvar _) = do
  state <- readMVar mvar
  case state of
    Idle -> internalBug "noticed state was idle when yielding"
    Running -> return ()
    Queued _ -> throwIO GracefulExit

internalBug = error

data GracefulExit = GracefulExit
  deriving (Show, Typeable)

instance Exception GracefulExit
