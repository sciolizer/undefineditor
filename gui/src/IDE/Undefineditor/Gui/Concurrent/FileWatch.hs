module IDE.Undefineditor.Gui.Concurrent.FileWatch (
  FileWatcher(),
  startFileWatcher,
  stopFileWatcher,
  trySaveFile
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Graphics.UI.Gtk
import System.Directory
import System.GIO
import System.IO.Error

import IDE.Undefineditor.Util.Safe

-- todo: support watching file moves
data FileWatcher = FileWatcher FileMonitor FilePath (MVar (Maybe String)) (Maybe String -> Maybe String -> IO ())

startFileWatcher
  :: FilePath -- file path
  -> (Maybe String -> Maybe String -> IO ()) -- handler
  -> IO (FileWatcher, Maybe String) -- (watcher, first result)
startFileWatcher fp handler = do
  putStrLn $ "starting File watcher for " ++ show fp
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

stopFileWatcher :: FileWatcher -> IO ()
stopFileWatcher (FileWatcher mon _fp mvar _handle) = withMVar mvar $ \_ -> do
  cancelFileMonitor mon
  return ()

cancelFileMonitor fileMonitor = do
  b <- fileMonitorCancel fileMonitor
  unless b $ putStrLn "cancelling of file monitor was unsuccessful"

-- data SaveResult = Saved | SyncError | IOErr IOException
-- todo: make sure a good chunk of the io exceptions that can be raised by trySaveFile are handled appropriately

-- attempts to save the given file. will not succeed (i.e. will return false without saving anything) if the file has been modified since the last time the relevant watchHandle has been raised. If the operation fails for any reason other than a sync reason, the io exception will be propogated as normal.
-- no checking is done to make sure that the given filewatcher has not already been cancelled
-- if this returns false, that indicates that the handler was invoked, and you should try again with the new value
trySaveFile :: FileWatcher -> Maybe String -> IO Bool
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
