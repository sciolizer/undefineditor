module IDE.Undefineditor.Util.RefCountMapIO (
  RefCountMapIO(),
  newRefCountMapIO,
  getOrCreate,
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import IDE.Undefineditor.Util.Delay
import IDE.Undefineditor.Util.Disposable
import IDE.Undefineditor.Util.Reactive
import IDE.Undefineditor.Util.RefCountMap

data RefCountMapIO k v = RefCountMapIO (k -> Disposable v) (MVar (RefCountMap k (v, IO ())))

newRefCountMapIO :: (k -> Disposable v) -> IO (RefCountMapIO k v)
newRefCountMapIO cons = do
  mvar <- newMVar empty
  return (RefCountMapIO cons mvar)

getOrCreate :: (Ord k) => RefCountMapIO k v -> k -> Disposable v
getOrCreate (RefCountMapIO cons mvar) k = ret where
  ret = modifyMVar mvar $ \mp -> do
          (i, (v, destr), mp') <- increment k (cons k) mp
          freer <- limit1 (free destr)
          return (mp', (v, freer))
  free destr = modifyMVar_ mvar $ \rcm -> do
    let (j, _, rcm') = decrement k rcm
    when (j == 0) $ destr
    return rcm'

limit1 :: IO () -> IO (IO ())
limit1 action = do
  mvar <- newMVar ()
  return $ do
    mb <- tryTakeMVar mvar
    case mb of
      Nothing -> throwIO (userError "action already executed")
      Just _ -> action
