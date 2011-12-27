{-# LANGUAGE
 TupleSections
 #-}
module IDE.Undefineditor.Lib (
  DisposableStream,
  LibStream,

  once,
  Lib(),
  newLib
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
import System.IO.Error

import IDE.Undefineditor.Util.Delay
import IDE.Undefineditor.Util.Reactive
import IDE.Undefineditor.Util.Streams

data Lib = Lib {
  libStreams :: Streams
  }

-- todo: allow for an overlay in 'once'
once :: LibStream a -> IO a
once libs = do
  lib <- newLib rigidStream rigidDirRead
  (ret', free) <- libs lib
  ret <- atomically . peekStream $ ret'
  free
  return ret

{-
withTempDelay :: (Delay -> IO a) -> IO a
withTempDelay f = bracket (newTVarIO (Just (return ()))) fireCleanup (f <=< newDelay . mkFork) where
  fireCleanup tvar = onJust tvar (\act -> (Nothing, act))
  mkFork tvar action = onJust tvar (\act -> (Just (act >> action), myThreadId))
  -}

onJust :: TVar (Maybe (IO ())) -> (IO () -> (Maybe (IO ()), IO a)) -> IO a
onJust tvar f = join $ atomically $ do
  v <- readTVar tvar
  case v of
    Nothing -> throwSTM (userError "internal bug: temp delay is already nothing")
    Just act -> writeTVar tvar toWrite >> return toReturn where
      (toWrite, toReturn) = f act

{-
withBlocker :: (STM () -> IO a) -> IO a
withBlocker f = bracket (newTVarIO False) (atomically . (`writeTVar` True)) (f . mkTrans) where
  mkTrans tvar = do
    v <- readTVar tvar
    unless v retry
    -}

pureStream :: a -> (Stream a, IO ())
pureStream v = (return v, return ())

rigidStream :: FilePath -> DisposableStream (Maybe String)
rigidStream fp = pureStream `fmap` readWholeFile fp

rigidDirRead :: FilePath -> DisposableStream (Maybe [FilePath])
rigidDirRead fp = pureStream `fmap` readWholeDir fp

readWholeDir :: FilePath -> IO (Maybe [FilePath])
readWholeDir fp = (Just `fmap` ret) `catchIOError` nothingOnNotExist where
  ret = (map (fp </>) . filter (\z -> z /= "." && z /= "..")) `fmap` getDirectoryContents fp

type LibStream a = Lib -> DisposableStream a

-- | Usually called once per application run, for consumers wishing to use
-- this library statefully. Consumers wishing to use the stateless
-- approach should instead use 'once' on each function from this module.
newLib :: (FilePath -> DisposableStream (Maybe String)) -> (FilePath -> DisposableStream (Maybe [FilePath])) -> IO Lib
newLib streamFile streamDir = liftM Lib $ newStreams streamFile streamDir

readWholeFile :: FilePath -> IO (Maybe String)
readWholeFile fp = (Just `fmap` ret) `catchIOError` nothingOnNotExist where
  ret = do
    s <- readFile fp
    evaluate (length s)
    return s

nothingOnNotExist e = if isDoesNotExistError e then return Nothing else ioError e
