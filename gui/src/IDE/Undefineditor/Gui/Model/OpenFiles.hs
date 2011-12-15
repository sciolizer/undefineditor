module IDE.Undefineditor.Gui.Model.OpenFiles (
  OpenFiles(),
  FileId(),
  Saved(..),
  isFileSaved,

  newOpenFiles,
  getFilePath,
  openFile,
  saveDirtyFiles,
  closeFile

) where

import Control.Concurrent hiding (yield)
import Control.Monad
import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Unique
import Graphics.UI.Gtk hiding (on)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView (
  SourceBuffer(),
  sourceBufferNew,
  sourceBufferNewWithLanguage,
  sourceLanguageManagerGetDefault,
  sourceLanguageManagerGetLanguageIds,
  sourceLanguageManagerGuessLanguage
  -- sourceViewNewWithBuffer,
  -- sourceViewShowLineNumbers
  )

import IDE.Undefineditor.Gui.Concurrent.Background
import IDE.Undefineditor.Gui.Concurrent.FileWatch
import IDE.Undefineditor.Gui.Controller.MRVar
import IDE.Undefineditor.Gui.Controller.Reactive
import IDE.Undefineditor.Gui.Util.TextBuffer
import IDE.Undefineditor.Util.Fork
import IDE.Undefineditor.Util.Safe

data Saved = Dirty | Clean
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data OpenFiles = OpenFiles (MRVar (M.Map FilePath (S.Set FileId, SourceBuffer, FileWatcher, Saved))) (IO ())

isFileSaved :: OpenFiles -> FilePath -> RRead (Maybe Saved)
isFileSaved (OpenFiles mrvar _) fp = do
  mp <- readMRVar mrvar
  case M.lookup fp mp of
    Nothing -> return Nothing
    Just (_, _, _, s) -> return (Just s)

data FileId = FileId Unique OpenFiles FilePath (IO ())

identity (FileId u _ _ _) = u

getFilePath (FileId _ _ fp _) = fp

instance Eq FileId where (==) = (==) `on` identity
instance Ord FileId where compare = compare `on` identity

newOpenFiles :: RVars -> IO OpenFiles
newOpenFiles rvars = do
  ret <- newMRVar rvars M.empty
  background <- newBackground fork -- todo: is forkOS the right thing to do?
  let saveall = launchBackground background $ do
          -- todo: this thread should give up waiting if the application is shutting down
          putStrLn "starting thread delay"
          threadDelay 3000000 -- 3 seconds; but may be as much as 6 seconds if a yield happens
          putStrLn "finishing thread delay; about to yield"
          yield background
          putStrLn "yield was uneventful; proceeding with saving of files"
          postGUIAsync (cleanly rvars $ saveDirtyFiles_ ret)
  return (OpenFiles ret saveall)

saveDirtyFiles :: OpenFiles -> IO ()
saveDirtyFiles (OpenFiles mvar _) = saveDirtyFiles_ mvar

saveDirtyFiles_ mvar = modifyMRVar_ mvar $ \mp ->
  return . M.fromList =<< mapM saveFile (M.toList mp)

saveFile x@(fp, (_, _, _, Clean)) = do
  putStrLn $ "skipping over clean file: " ++ show fp
  return x
saveFile (fp, (fids, tb, fw, Dirty)) = do
  putStrLn $ "resaving file: " ++ show fp
  text <- textBufferGetContents tb
  b <- trySaveFile fw (Just text)
  unless b $ putStrLn $ "saving file was unsuccessful: " ++ show fp
  return (fp, (fids, tb, fw, Clean))

-- todo: do some canonicalization of the file path; GIO seems to have pretty good support for this
openFile
  :: OpenFiles
  -> FilePath
  -> IO () -- callback for when this file gets deleted
  -> IO (Maybe (FileId, SourceBuffer))
openFile o@(OpenFiles mvar saveall) fp deletion = modifyMRVar mvar $ \mp ->
  case M.lookup fp mp of
    Nothing -> do
      tb <- mkSourceBuffer fp
      (fw, contents) <- startFileWatcher fp $ \_oldFile newFile ->
        case newFile of
          Nothing -> cleanly (getMRVars mvar) $
            modifyMRVar_ mvar $ \mp ->
              case M.lookup fp mp of
                Nothing -> internalBug "got file deletion notification but file is not in map"
                Just (fids, _tb, fw, saved) -> do
                  stopFileWatcher fw
                  mapM_ (\(FileId _ _ _ a) -> safe a) (S.toList fids)
                  -- textBufferSetText tb "this file has been deleted" -- dangerous, but will help us discover where the deletion callbacks cannot be trusted
                  case saved of
                    Clean -> return ()
                    Dirty -> putStrLn "file changes lost: file deleted in file system"
                  return (M.delete fp mp)
          Just contents -> textBufferSetText tb contents
      case contents of
        Nothing -> do
          stopFileWatcher fw
          return (mp, Nothing)
        Just z -> do
          textBufferSetText tb z
          Gtk.on tb bufferChanged $ cleanly (getMRVars mvar) $ do
            putStrLn "bufferchanged"
            modifyMRVar_ mvar $ \mp ->
              case M.lookup fp mp of
                Nothing -> do
                  putStrLn $ "buffer changed, but file is no longer in map: " ++ show fp
                  return mp
                Just (fids, tb, fw, _) -> do
                  putStrLn "saving all"
                  saveall
                  putStrLn "called saving all and returing file marked as dirty"
                  return (M.insert fp (fids, tb, fw, Dirty) mp)
            putStrLn "mvar set to dirty to match bufferchanged"
          u <- newUnique
          let fid = FileId u o fp deletion
          putStrLn "successfully created new text buffer"
          return (M.insert fp (S.singleton fid, tb, fw, Clean) mp, Just (FileId u o fp deletion, tb))
    Just (fids, tb, fw, saved) -> do
      u <- newUnique
      let fid = FileId u o fp deletion
      return (M.insert fp (S.insert fid fids, tb, fw, saved) mp, Just (fid, tb))

closeFile :: FileId -> IO ()
closeFile f@(FileId _ (OpenFiles mvar _) fp _) = modifyMRVar_ mvar $ \mp ->
  case M.lookup fp mp of
    Nothing -> return mp
    Just (fids, tb, fw, saved) -> do
      let fids' = S.delete f fids
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
  print =<< sourceLanguageManagerGetLanguageIds manager
  language <- guessLanguage manager path
  case language of
    Just lan -> sourceBufferNewWithLanguage lan
    Nothing -> sourceBufferNew Nothing

guessLanguage manager path = sourceLanguageManagerGuessLanguage manager (Just path) Nothing
