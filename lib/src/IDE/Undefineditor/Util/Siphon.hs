{-# LANGUAGE
 DoRec,
 ScopedTypeVariables,
 TupleSections
 #-}
module IDE.Undefineditor.Util.Siphon (
  siphon
) where

import Control.Concurrent.STM
import Control.Monad

import IDE.Undefineditor.Util.Disposable
import IDE.Undefineditor.Util.Reactive

-- regretably, there is a slight delay in propogation.
-- The looping nature of 'cleanly' ensures that the
-- momentary inconsistency is resolved before 'cleanly' returns.
{-
siphon :: RVars -> (a -> a -> Bool) -> (a -> IO b) -> Stream a -> IO (Stream b)
siphon rvars cmp f s = do
  rec
    rvar <- newRVarIO rvars =<< f val
    Just val <- atomically $ react (Just `fmap` s) (\o n -> unless (cmp o n) (atomically . writeRVar rvar =<< f n))
  return (readRVar rvar)
  -}

siphon :: forall a b. RVars -> (a -> a -> Bool) -> Stream a -> (a -> Disposable (Stream b)) -> Disposable (Stream b)
siphon rvars eq ins pump = do
  inUse <- newRVarIO rvars True
  rec
    rvar <- newRVarIO rvars =<< pump val
    Just val <- atomically $ react (ins `while` inUse) (\o n -> unless (eq o n) $ do
      -- we don't want to create a closer that will never be freed, so we double check
      -- the inUse variable before invoking pump. (It is possible for the react callback to fire
      -- after inUse has been set to False, if cleanly has not yet been called.)
      u <- atomically . peekStream . readRVar $ inUse
      when u (reassign rvar =<< pump n))
  let retStream = do (st, _) <- readRVar rvar
                     st
  return . ((retStream :: Stream b),) . join . atomically $ do
    i <- peekStream $ readRVar inUse
    if i then do
      writeRVar inUse False
      snd `fmap` peekStream (readRVar rvar)
    else return (return ()) -- closer has already been called; do nothing

reassign :: RVar (b, IO ()) -> (b, IO ()) -> IO ()
reassign rvar new = join . atomically $ do
  (_, fOld) <- peekStream $ readRVar rvar
  writeRVar rvar new
  return fOld

while :: Stream a -> RVar Bool -> Stream (Maybe a)
while st rvar = do
  v <- readRVar rvar
  liftM (if v then Just else const Nothing) st
