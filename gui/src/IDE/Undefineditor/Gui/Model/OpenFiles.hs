-- | Tracks all open files, saves unsaved files periodically, reloads files that have been modified
-- on disk, and shares 'SourceBuffer's between multiple tabs.
module IDE.Undefineditor.Gui.Model.OpenFiles (
  -- * Open file collection
  OpenFiles(),
  newOpenFiles,

  -- * Individual files
  FileId(),
  openFile,
  getFilePath,
  closeFile,

  -- * Saving files
  Saved(..),
  isFileSaved,
  saveDirtyFiles

) where

import Control.Concurrent (forkOS, threadDelay)
import Control.Monad (unless)
import Data.Function (on)
import qualified Data.Map as M (Map(), delete, empty, fromList, insert, lookup, toList)
import qualified Data.Set as S (Set(), delete, insert, null, singleton)
import Data.Unique (Unique(), newUnique)
import Graphics.UI.Gtk (bufferChanged, postGUIAsync, textBufferSetText)
import qualified Graphics.UI.Gtk as Gtk (on)
import Graphics.UI.Gtk.SourceView (
  SourceBuffer(),
  sourceBufferNew,
  sourceBufferNewWithLanguage,
  sourceLanguageManagerGetDefault,
  sourceLanguageManagerGuessLanguage
  )

import IDE.Undefineditor.Gui.Concurrent.Background
import IDE.Undefineditor.Gui.Concurrent.FileWatch
import IDE.Undefineditor.Gui.Controller.MRVar
import IDE.Undefineditor.Gui.Controller.Reactive
import IDE.Undefineditor.Gui.Util.TextBuffer

-- | Whether a file has been modified since it's last save.
data Saved =
    Dirty -- ^ File has been modified since it was last saved.
  | Clean -- ^ Buffer and file system are in-sync.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A collection of files.
data OpenFiles = OpenFiles (MRVar (M.Map FilePath (S.Set FileId, SourceBuffer, FileWatcher, Saved))) (IO ())

-- | Determines whether the given file is dirty or clean. Returns 'Nothing' if the 'FileId' has been closed or deleted.
isFileSaved :: FileId -> Stream (Maybe Saved)
isFileSaved fid = do
  let OpenFiles mrvar _ = openFiles fid
  let fp = getFilePath fid
  mp <- readMRVar mrvar
  case M.lookup fp mp of
    Nothing -> return Nothing
    Just (_, _, _, s) -> return (Just s)

-- | A handle to a file. Corresponds to a tab in the interface. If two 'FileId's are equal, then
-- their 'FilePath's are equal, but the converse is not necessarily true.
data FileId = FileId {
  identity :: Unique,
  openFiles :: OpenFiles,
  -- | Gets the 'FilePath' that the given 'FileId' was constructed with.
  getFilePath :: FilePath }

instance Eq FileId where (==) = (==) `on` identity
instance Ord FileId where compare = compare `on` identity

-- | Instantiates a new (empty) collection of 'OpenFiles', and creates a 'forkOS' 'Background'
-- thread, which periodically saves modified files.
newOpenFiles :: RVars -> IO OpenFiles
newOpenFiles rvars = do
  -- We must use forkOS instead of forkIO because the haskell RTS cannot preempt gtk's
  -- event loop.
  background <- newBackground forkOS
  ret <- newMRVar rvars M.empty
  let saveall = launchBackground background $ do
          -- todo: this thread should give up waiting if the application is shutting down
          threadDelay 1000000 -- 1 second; but may be as much as 2 seconds if a yield happens
            -- todo: (not really necessary, but could be fun... add a yieldFor :: Background -> Int -> IO () function to the Background module, so that we don't end up with this weird 2 second behavior)
          yield background
          postGUIAsync (cleanly rvars $ saveDirtyFiles_ ret)
  return (OpenFiles ret saveall)

-- | Saves all files in the 'OpenFiles' collection that have not been saved.
saveDirtyFiles :: OpenFiles -> IO ()
saveDirtyFiles (OpenFiles mvar _) = saveDirtyFiles_ mvar

saveDirtyFiles_ mvar = modifyMRVar_ mvar $ \mp ->
  return . M.fromList =<< mapM saveFile (M.toList mp)

saveFile x@(_, (_, _, _, Clean)) = return x
saveFile (fp, (fids, tb, fw, Dirty)) = do
  text <- textBufferGetContents tb
  b <- trySaveFile fw (Just text)
  unless b $ warning $ "saving file was unsuccessful: " ++ show fp
  return (fp, (fids, tb, fw, Clean))

-- todo: do some canonicalization of the file path; GIO seems to have pretty good support for this

-- | Constructs a new 'SourceBuffer' for the given 'FilePath' if one does not already exist, and
-- registers the file for background saves.
openFile
  :: OpenFiles -- ^ Collection of currently open files.
  -> FilePath -- ^ File to open
  -> IO (Maybe (FileId, SourceBuffer)) -- ^ returns 'Nothing' if the file does not exist. Otherwise returns an identity for this call to 'openFile' along with a 'SourceBuffer', which will be newly constructed only if this file has not been opened before.
openFile o@(OpenFiles mvar saveall) fp = modifyMRVar mvar $ \mp ->
  case M.lookup fp mp of
    Nothing -> do
      tb <- mkSourceBuffer fp
      (fw, contents) <- startFileWatcher fp $ \_oldFile newFile ->
        case newFile of
          Nothing -> cleanly (getMRVars mvar) $
            modifyMRVar_ mvar $ \mp ->
              case M.lookup fp mp of
                Nothing -> internalBug "got file deletion notification but file is not in map"
                Just (_fids, _tb, fw, saved) -> do
                  stopFileWatcher fw
                  -- textBufferSetText tb "this file has been deleted" -- dangerous, but will help us discover where the deletion callbacks cannot be trusted
                  case saved of
                    Clean -> return ()
                    Dirty -> warning "file changes lost: file deleted in file system"
                  return (M.delete fp mp)
          Just contents -> textBufferSetText tb contents
      case contents of
        Nothing -> do
          stopFileWatcher fw
          return (mp, Nothing)
        Just z -> do
          textBufferSetText tb z
          Gtk.on tb bufferChanged $ cleanly (getMRVars mvar) $ do
            modifyMRVar_ mvar $ \mp ->
              case M.lookup fp mp of
                Nothing -> do
                  warning $ "buffer changed, but file is no longer in map: " ++ show fp
                  return mp
                Just (fids, tb, fw, _) -> do
                  saveall
                  return (M.insert fp (fids, tb, fw, Dirty) mp)
          u <- newUnique
          let fid = FileId u o fp
          return (M.insert fp (S.singleton fid, tb, fw, Clean) mp, Just (fid, tb))
    Just (fids, tb, fw, saved) -> do
      u <- newUnique
      let fid = FileId u o fp
      return (M.insert fp (S.insert fid fids, tb, fw, saved) mp, Just (fid, tb))

-- | Removes the given 'FileId' from the 'OpenFiles' collection that it belongs to. If there are no
-- other open 'FileId's with the same 'FilePath', then this function will also save the file (if saving is necessary).
closeFile :: FileId -> IO ()
closeFile fid = modifyMRVar_ mvar closer where
  OpenFiles mvar _ = openFiles fid
  fp = getFilePath fid
  closer mp =
    case M.lookup fp mp of
      Nothing -> return mp
      Just (fids, tb, fw, saved) -> do
        let fids' = S.delete fid fids
        if S.null fids' then
          do
            stopFileWatcher fw
            saveFile (fp, (fids', tb, fw, saved))
            return (M.delete fp mp)
          else return (M.insert fp (fids', tb, fw, saved) mp)

internalBug = error

mkSourceBuffer path = do
  -- todo: what is glib filename encoding?
  -- todo: if guessing the source language doesn't work, then make a notification to the user that something needs to be installed
  manager <- sourceLanguageManagerGetDefault
  language <- guessLanguage manager path
  case language of
    Just lan -> sourceBufferNewWithLanguage lan
    Nothing -> sourceBufferNew Nothing

guessLanguage manager path = sourceLanguageManagerGuessLanguage manager (Just path) Nothing

warning = putStrLn
