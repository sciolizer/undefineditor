-- | A library for running cleanup operations, such
-- as closing files when they are no longer in use.
module IDE.Undefineditor.Util.Delay (
  Delay(),
  newDelay,
  delay
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import IDE.Undefineditor.Util.Safe

-- | A service which executes actions only when they are ready to be run.
data Delay = Delay (IO () -> IO ThreadId) (TVar [Action])

type Action = (STM (), IO ())

-- | Creates a delay service.
newDelay :: (IO () -> IO ThreadId) -> IO Delay
newDelay fork = do
  actions <- newTVarIO []
  return (Delay fork actions)

-- | Runs the given action when the given transaction has unblocked.
-- Although the actions will usually run on the same thread as all
-- other calls to delay on this 'Delay' instance, they will sometimes
-- be run concurrently (particular when the number of registered delay
-- actions is thrashing between zero and one), so the given action should
-- be able to run concurrently with other actions.
--
-- The given action should not raise any exceptions.
delay :: Delay -> STM () -> IO () -> IO ()
delay (Delay fork actions) trans act = join $ atomically $ do
  acts <- readTVar actions
  case acts of
    [] -> do -- launches a new thread, since 
      writeTVar actions [(trans,act)]
      return . void . fork . consume $ actions
    xs -> do
      writeTVar actions ((trans,act):xs)
      return (return ())

consume :: TVar [Action] -> IO ()
consume tvar = do
  (action, remaining) <- atomically $ takeOne tvar
  safe action
  if null remaining then return () else consume tvar

takeOne :: TVar [Action] -> STM (IO (), [Action])
takeOne tvar = ret where
  ret = do
    t <- readTVar tvar
    if null t then return (return (), []) else do
    (act, mod) <- takeFrom t
    writeTVar tvar mod
    return (act, mod)
  takeFrom [] = retry
  takeFrom (x@(trans,act):xs) = takeFirst `orElse` (second (x:) <$> takeFrom xs) where
    takeFirst = do
       trans
       return (act, xs)
