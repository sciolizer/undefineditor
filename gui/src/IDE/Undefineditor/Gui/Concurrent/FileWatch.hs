-- | This module is a wrapper for 'fileMonitorFile'.
module IDE.Undefineditor.Gui.Concurrent.FileWatch (
  FileWatcher(),
  startFileWatcher,
  stopFileWatcher,
  trySaveFile
) where

import Control.Concurrent (MVar(), modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (evaluate)
import Control.Monad (join, unless, when)
import Graphics.UI.Gtk (on)
import System.Directory (doesFileExist, removeFile)
import System.GIO (FileMonitor(), fileFromURI, fileMonitorCancel, fileMonitorChanged, fileMonitorFile)
import System.IO.Error (isDoesNotExistError, tryIOError)

import IDE.Undefineditor.Util.Safe

-- todo: support watching file moves

-- | Watches the contents of a file, and runs a callback whenever the contents change. The callback is run on the Gtk gui thread.
data FileWatcher = FileWatcher FileMonitor FilePath (MVar (Maybe String)) (Maybe String -> Maybe String -> IO ())

-- | Starts watching the given file.
startFileWatcher
  :: FilePath -- ^ file to watch
  -> (Maybe String -> Maybe String -> IO ()) -- ^ callback to execute when contents change. First argument is old contents, second argument is new contents. 'Nothing' indicates the file does not exist.
  -> IO (FileWatcher, Maybe String) -- ^ new watcher and current contents of file. 'Nothing' indicates that the file does not exist.
startFileWatcher fp handler = do
  let fileRef = fileFromURI ("file://" ++ fp) -- todo: switch to fileFromPath and support unicode
  fileMonitor <- fileMonitorFile fileRef [{- todo: what is FileMonitorWatchMounts? -}] Nothing
  contents <- readWholeFile fp
  contentsMVar <- newMVar contents
  let safeHandler x y = safe (handler x y)
  on fileMonitor fileMonitorChanged $ \_mb1 _mb2 _event -> -- todo: look at event to know whether it's worth re-reading the file or not
    modifyMVar_ contentsMVar $ \oldContents -> do
      newContents <- readWholeFile fp
      when (oldContents /= newContents) $ safeHandler oldContents newContents
      return newContents
  return (FileWatcher fileMonitor fp contentsMVar safeHandler, contents)

-- | Stops watching the file associated with the given watcher.
stopFileWatcher :: FileWatcher -> IO ()
stopFileWatcher (FileWatcher mon _fp mvar _handle) = withMVar mvar $ \_ -> do
  cancelFileMonitor mon
  return ()

cancelFileMonitor fileMonitor = do
  b <- fileMonitorCancel fileMonitor
  unless b $ warning "cancelling of file monitor was unsuccessful"

warning = putStrLn

-- todo: make sure a good chunk of the io exceptions that can be raised by trySaveFile are handled appropriately

-- | Attempts to save to the file watched by the given watcher. The callback associated with the watcher will not fire as a result of calling this function. This function can be safely invoked from any thread, but it is only meaningful to call it from the gui thread. (If you call from another thread, then you have no protection against saving an old copy of the file.) If the file fails to save for any reason other than being out of sync (such as the disk being full), then the exception will be propogated to the caller.
trySaveFile
  :: FileWatcher -- ^ watcher representing the file to save. Behavior is undefined if the given watcher has been stopped.
  -> Maybe String -- ^ contents to be saved to file. 'Nothing' indicates that the file should be deleted
  -> IO Bool -- ^ returns True if contents were saved. False indicates that the contents of the file have changed, but the callback has not yet been executed; the caller should wait for the callback to fire and then retry, probably with different data.
trySaveFile (FileWatcher _mon fp mvar safeHandler) newContents = join $
  modifyMVar mvar $ \oldContents -> do
    checkedContents <- readWholeFile fp
    if oldContents == checkedContents then do
      case newContents of
        Nothing -> removeFile fp
        Just z -> writeFile fp z
      return (newContents, return True)
      else return (checkedContents, safeHandler oldContents checkedContents >> return False)

readWholeFile :: FilePath -> IO (Maybe String)
readWholeFile fp = do
  b <- doesFileExist fp -- shortcut check. Is checked again below (isDoesNotExistError)
  if b then do
    e <- tryIOError (readFile fp)
    case e of
      Left z -> if isDoesNotExistError z then return Nothing else ioError z
      Right r -> do
        evaluate (length r)
        return (Just r)
    else return Nothing
