module IDE.Undefineditor.Util.ReentrantLock (
  ReentrantLock(),
  ReentrantLockState(..),
  newReentrantLock,
  withLock,
  lockState
) where

import Control.Concurrent
-- import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import System.Exit

-- Despite the name, this is not really a re-entrant lock.
-- It WAS, until I discovered that Gtk signal handlers are invoked on different
-- haskell threads than the ones that triggered the signal. I suspect
-- the reason for this is that the FFI imports are marked safe instead of unsafe,
-- although I haven't checked to see if that's really the case.
--
-- So now I have no way to implement a re-entrant lock that works across Gtk callbacks.
-- Instead, this re-entrant lock merely ASSUMES that it is being used properly
-- (that is... no calls are ever made to withLock that would cause it to block),
-- and the implementation verifies it's assumptions on the release of the lock.
-- (specifically, it verifies that ThreadIds of acquires and releases are in
-- LIFO order). If a discrepency is ever found, the program is terminated.

newtype ReentrantLock = ReentrantLock (TVar [ThreadId])

data ReentrantLockState = Unlocked | LockedOnCurrentThread -- | LockedOnOtherThread
  deriving (Eq, Ord)

newReentrantLock :: IO ReentrantLock
newReentrantLock = ReentrantLock `fmap` newTVarIO []

-- bool indiciates if this is the first time this thread
-- has taken this lock (i.e. is false on reentry)
withLock :: ReentrantLock -> (Bool -> IO a) -> IO a
withLock (ReentrantLock tvar) action = do
  myTid <- myThreadId
  putStrLn $ "myThreadId: " ++ show myTid
  tids <- atomically $ do
    tids <- readTVar tvar
    writeTVar tvar (myTid:tids)
    return tids
  finally (action (null tids)) $ join $ atomically $ do
    tids' <- readTVar tvar
    case tids' of
      [] -> return failQuit
      (t:ts) | t == myTid && tids == ts -> writeTVar tvar tids >> return (return ())
             | otherwise -> return failQuit

failQuit = do
  putStrLn "withLock inconsistency found; terminating"
  exitWith (ExitFailure 1)

-- assumes that if locked, lock is on current thread
lockState :: ReentrantLock -> STM ReentrantLockState
lockState (ReentrantLock tvar) = do
  -- myTid <- myThreadId
  mbTid <- readTVar tvar
  case mbTid of
    [] -> return Unlocked
    _ -> return LockedOnCurrentThread
