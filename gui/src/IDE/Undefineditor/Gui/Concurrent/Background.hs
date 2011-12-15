{-# LANGUAGE
 DeriveDataTypeable
 #-}
module IDE.Undefineditor.Gui.Concurrent.Background (
  Background(),

  newBackground,
  launchBackground,
  yield

) where

import Prelude hiding (catch)

import Control.Concurrent (MVar(), ThreadId(), modifyMVar, newMVar, readMVar)
import Control.Exception (Exception(), catch, finally, fromException, throwIO)
import Control.Monad (join, void)
import Data.Typeable (Typeable())

data State =
    Idle
  | Running
  | Queued (IO ())

data Background = Background (MVar State) (IO () -> IO ThreadId)

newBackground :: (IO () -> IO ThreadId) -> IO Background
newBackground fork = do
  mvar <- newMVar Idle
  return (Background mvar fork)

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
