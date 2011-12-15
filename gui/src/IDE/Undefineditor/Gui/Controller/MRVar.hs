-- | Combines the behavior of 'MVar' and 'RVar', so that
-- you can manipulate 'RVar's with atomic guarantees.
module IDE.Undefineditor.Gui.Controller.MRVar (
  MRVar(),
  newMRVar,
  getMRVars,
  readMRVar,
  modifyMRVar,
  modifyMRVar_
) where

import Control.Concurrent.MVar (MVar(), modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (atomically)

import IDE.Undefineditor.Gui.Controller.Reactive

-- | An 'MVar' and an 'RVar'.
data MRVar a = MRVar (MVar a) (RVar a)

-- | Creates a new 'MRVar', with the given initial value.
newMRVar :: RVars -> a -> IO (MRVar a)
newMRVar rvars x = do
  m <- newMVar x
  r <- newRVarIO rvars x
  return (MRVar m r)

-- | Returns the 'RVars' that the 'MRVar' was created with.
getMRVars :: MRVar a -> RVars
getMRVars (MRVar _ rv) = getRVars rv

-- | Reads the value of the 'MRVar'.
readMRVar :: MRVar a -> Stream a
readMRVar (MRVar _ r) = readRVar r

-- | Analogue of 'modifyMVar_'. This should only be called in the context of a 'cleanly'.
modifyMRVar_ :: MRVar a -> (a -> IO a) -> IO ()
modifyMRVar_ (MRVar m r) f = modifyMVar_ m $ \mv -> do
  mv' <- f mv
  atomically $ writeRVar r mv'
  return mv'

-- | Analogue of 'modifyMVar'. This should only be called in the context of a 'cleanly'.
modifyMRVar :: MRVar a -> (a -> IO (a, b)) -> IO b
modifyMRVar (MRVar m r) f = modifyMVar m $ \mv -> do
  p@(mv', _) <- f mv
  atomically $ writeRVar r mv'
  return p

