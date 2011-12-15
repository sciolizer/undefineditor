{-# LANGUAGE
 DoRec,
 RankNTypes
 #-}
module IDE.Undefineditor.Gui.Controller.Tabs (
  Tabs(),
  newTabs,
  openTabFile,
  closeAllTabs,
  closeCurrentTab,
  getCurrentTab,
  nextTab,
  previousTab
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.SourceView
import System.FilePath

import IDE.Undefineditor.Gui.Controller.MRVar
import IDE.Undefineditor.Gui.Controller.Reactive
import qualified IDE.Undefineditor.Gui.Model.FocusedMap as FM
import IDE.Undefineditor.Gui.Model.OpenFiles
import IDE.Undefineditor.Gui.View.Notebook

-- todo: canonicalization of filepath
data Tabs = Tabs OpenFiles Notebook (MRVar (FM.FocusedMap FilePath (FileId, EditorTab))) (forall a. IO a -> IO a)

data FocusSync = Paused | Going

-- called once per window
newTabs :: RVars -> OpenFiles -> Notebook -> IO Tabs
newTabs rvars o n = do
  mvar <- newMRVar rvars FM.empty
  fs <- newMVar Going
  on n switchPage $ \i -> do
    myTid <- myThreadId
    putStrLn $ "switch page handler: " ++ show myTid
    cleanly rvars $ do
      putStrLn "attempting to change focus on switching of page"
      withMVar fs $ \g ->
        case g of
          Going -> modifyMRVar_ mvar (return . FM.setFocus i)
          Paused -> return ()
      putStrLn "finished attempting"
  return (Tabs o n mvar (paused fs))

openTabFile :: Tabs -> FilePath -> IO ()
openTabFile (Tabs o n mvar paused) fp = modifyMRVar_ mvar $ \fm ->
  case FM.locateIndex fp fm of
    Just i -> do
      notebookSetCurrentPage n i -- sync point
      return (FM.setFocus i fm)
    Nothing -> do
      mb <- openFile o fp (putStrLn "todo: handling file deletion not yet implemented")
      case mb of
        Nothing -> do
          putStrLn ("file does not exist: " ++ show fp)
          return fm -- file does not exist; do nothing
        Just (fid, tb) -> do
          putStrLn "file does exist; proceeding to create a new tab"
          rec
            let fm' = FM.insert fp (fid, editorTab) fm
            let Just i = FM.locateIndex fp fm'
            editorTab <- paused $ newTab n i tb fp
            return ()
          -- sync point
          paused $ do
            notebookSetCurrentPage n i
            let trigger = do
                  sd <- isFileSaved o fp
                  case sd of
                    Nothing -> return Nothing
                    Just saved -> do
                      allFileNames <- (map fst . FM.toAscList) `fmap` readMRVar mvar
                      return (Just (saved, allFileNames))
            let update (saved, allFileNames) = do
                  let mod Clean = id
                      mod Dirty = ('*':)
                  notebookSetTabLabelText n (tabWidget editorTab) (mod saved (unamb fp allFileNames))
            mbSaved <- react trigger $ \old new -> when (old /= new) (update new)
            case mbSaved of
              Nothing -> bug "created tab for not existent file, despite what openFiles just told me"
              Just x -> update x
          putStrLn "created a new tab"
          return (FM.setFocus i fm')

paused fs = bracket_ (modifyMVar_ fs (\_ -> return Paused)) (modifyMVar_ fs (\_ -> return Going))

getCurrentTab :: Tabs -> IO (Maybe (FileId, EditorTab))
getCurrentTab (Tabs _ _ mvar _) = do
  fm <- atomically . liftRRead . readMRVar $ mvar
  case FM.lookupFocus fm of
    Nothing -> return Nothing
    Just (_fp, (fid, tb)) -> return (Just (fid, tb))

closeCurrentTab :: Tabs -> IO ()
closeCurrentTab (Tabs _o n mvar paused) = modifyMRVar_ mvar (closeCurrentTab' n paused)

closeCurrentTab' n paused fm =
  case (FM.getFocus fm, FM.lookupFocus fm) of
    (Nothing, Nothing) -> return fm
    (Just i, Just (_fp, (fid, _tb))) -> do
      paused $ notebookRemovePage n i
      closeFile fid -- todo: save? although that should probably be done by OpenFiles
      return (FM.deleteFocus fm)
    _ -> todo "change interface of FM so that I don't have to handle this impossible case"

closeAllTabs :: Tabs -> IO ()
closeAllTabs (Tabs _ n mvar paused) = modifyMRVar_ mvar rep where
  rep fm = foldM (\prev _ -> closeCurrentTab' n paused prev) fm [1..(FM.size fm)]

nextTab :: Tabs -> IO ()
nextTab = changeTab 1

changeTab inc (Tabs _ n mvar paused) = modifyMRVar_ mvar $ \fm -> do
  let fm' = modFocus inc fm
  let f = FM.getFocus fm'
  case f of
    Nothing -> return fm'
    Just i -> do
      paused $ notebookSetCurrentPage n i
      return fm'

modFocus inc tabs =
  case FM.getFocus tabs of
    Nothing -> tabs
    Just i -> FM.setFocus ((i + inc) `mod` FM.size tabs) tabs

previousTab :: Tabs -> IO ()
previousTab = changeTab (-1)

unamb :: FilePath -> [FilePath] -> String
unamb fp set = ret where
  parts = splitPath fp
  (_:opts) = reverse (tails parts)
  options = map joinPath opts
  remaining = filter (/= fp) set
  ret = fromMaybe (last options) (find (\op -> all (\s -> not (op `isSuffixOf` s)) remaining) options)

todo = error

bug = error
