-- | Despite the name, this is not really a re-entrant lock.
-- It WAS, until I discovered that Gtk signal handlers are invoked on different
-- haskell threads than the ones that triggered the signal. I suspect
-- the reason for this is that the FFI imports are marked safe instead of unsafe,
-- although I haven't checked to see if that's really the case.
--
-- So now I have no way to implement a re-entrant lock that works across Gtk callbacks.
-- Instead, this re-entrant lock merely ASSUMES that it is being used properly
-- (that is... we assume no calls are ever made to withLock that would cause it to block),
-- and the implementation verifies its assumptions on the release of the lock.
-- (specifically, it verifies that ThreadIds of acquires and releases are in
-- LIFO order). If a discrepency is ever found, the program is terminated.
module IDE.Undefineditor.Util.ReentrantLock (
  ReentrantLock(),
  ReentrantLockState(..),
  newReentrantLock,
  withLock,
  lockState
) where

import Control.Concurrent (ThreadId(), myThreadId)
import Control.Concurrent.STM (STM(), TVar(), atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (finally)
import Control.Monad (join)
import System.Exit (ExitCode(ExitFailure), exitWith)


-- | A stack of 'ThreadId's.
newtype ReentrantLock = ReentrantLock (TVar [ThreadId])

-- | Current state of the lock.
data ReentrantLockState =
    Unlocked -- ^ nobody is currently inside lock
  | Locked -- ^ somebody is currently inside lock
  deriving (Eq, Ord)

-- | Creates a new empty 'ReentrantLock'
newReentrantLock :: IO ReentrantLock
newReentrantLock = ReentrantLock `fmap` newTVarIO []

-- | Executes the given action in the context of the given ReentrantLock. The boolean passed
-- to the action is True if the lock was previously Unlocked, and False if this usage is
-- re-entrant.
withLock :: ReentrantLock -> (Bool -> IO a) -> IO a
withLock (ReentrantLock tvar) action = do
  myTid <- myThreadId
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

-- | Returns whether the lock is acquired or not.
lockState :: ReentrantLock -> STM ReentrantLockState
lockState (ReentrantLock tvar) = do
  mbTid <- readTVar tvar
  case mbTid of
    [] -> return Unlocked
    _ -> return Locked
