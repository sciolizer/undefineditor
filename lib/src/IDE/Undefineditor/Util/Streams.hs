module IDE.Undefineditor.Util.Streams (
  Streams(),
  DisposableStream,
  newStreams,
  fileStream,
  dirStream
) where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad

import IDE.Undefineditor.Util.Delay
import IDE.Undefineditor.Util.Disposable
import IDE.Undefineditor.Util.Reactive
import IDE.Undefineditor.Util.RefCountMapIO

type DisposableStream a = Disposable (Stream a)

data Streams = Streams (RefCountMapIO FilePath (Stream (Maybe String))) (RefCountMapIO FilePath (Stream (Maybe [FilePath])))

newStreams :: (FilePath -> DisposableStream (Maybe String)) -> (FilePath -> DisposableStream (Maybe [FilePath])) -> IO Streams
newStreams fileStreamer dirStreamer = do
  f <- newRefCountMapIO fileStreamer
  d <- newRefCountMapIO dirStreamer
  return (Streams f d)

fileStream :: Streams -> FilePath -> IO (Stream (Maybe String), IO ())
fileStream (Streams f _) fp = getOrCreate f fp

dirStream :: Streams -> FilePath -> IO (Stream (Maybe [FilePath]), IO ())
dirStream (Streams _ d) fp = getOrCreate d fp
