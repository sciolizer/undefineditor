module IDE.Undefineditor.Gui.Controller.MRVar (
  MRVar(),
  newMRVar,
  getMRVars,
  readMRVar,
  modifyMRVar,
  modifyMRVar_
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM

import IDE.Undefineditor.Gui.Controller.Reactive

data MRVar a = MRVar (MVar a) (RVar a)

newMRVar :: RVars -> a -> IO (MRVar a)
newMRVar rvars x = do
  m <- newMVar x
  r <- newRVarIO rvars x
  return (MRVar m r)

getMRVars :: MRVar a -> RVars
getMRVars (MRVar _ rv) = getRVars rv

readMRVar :: MRVar a -> RRead a
readMRVar (MRVar _ r) = readRVar r

modifyMRVar_ :: MRVar a -> (a -> IO a) -> IO ()
modifyMRVar_ (MRVar m r) f = modifyMVar_ m $ \mv -> do
  mv' <- f mv
  atomically $ writeRVar r mv'
  return mv'

modifyMRVar :: MRVar a -> (a -> IO (a, b)) -> IO b
modifyMRVar (MRVar m r) f = modifyMVar m $ \mv -> do
  p@(mv', _) <- f mv
  atomically $ writeRVar r mv'
  return p

