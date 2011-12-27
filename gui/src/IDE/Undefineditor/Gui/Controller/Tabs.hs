{-# LANGUAGE
 DoRec,
 RankNTypes
 #-}

-- | Controller for the tabs in a window's notebook.
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

import Control.Concurrent (modifyMVar_, newMVar, withMVar)
import Control.Concurrent.STM (atomically)
import Control.Exception (bracket_)
import Control.Monad (foldM)
import Data.List (find, isSuffixOf, tails)
import Data.Maybe (fromMaybe)
import Graphics.UI.Gtk (Notebook(), notebookSetCurrentPage, notebookRemovePage, on, switchPage)
import System.FilePath (joinPath, splitPath)

import IDE.Undefineditor.Gui.Controller.MRVar
import IDE.Undefineditor.Util.Reactive
import qualified IDE.Undefineditor.Gui.Model.FocusedMap as FM
import IDE.Undefineditor.Gui.Model.OpenFiles
import IDE.Undefineditor.Gui.View.Notebook

-- todo: canonicalization of filepath
-- todo: figure out how to get rid of this file. It's turning into a big ball of mud. I think I created this file back before the 'cleanly' function existed... with the existence of the 'cleanly' function, I might be able to go back to a controller-free management of the tabs. But be careful. There's definitely a lot of complexity here.

-- | Collection of tabs in a window.
data Tabs = Tabs OpenFiles Notebook (MRVar (FM.FocusedMap FilePath (FileId, HaskellTab))) (forall a. IO a -> IO a)

data FocusSync = Paused | Going

-- | Constructs an empty collection of tabs.
newTabs :: RVars -> OpenFiles -> Notebook -> IO Tabs
newTabs rvars o n = do
  mvar <- newMRVar rvars FM.empty
  fs <- newMVar Going
  on n switchPage $ \i -> do
    cleanly rvars $ do
      withMVar fs $ \g ->
        case g of
          Going -> modifyMRVar_ mvar (return . FM.setFocus i)
          Paused -> return ()
  return (Tabs o n mvar (paused fs))

-- | Ensures that the given file path is in the collection of tabs. If the given file is already
-- open in a tab, then focus is switched to that tab.
openTabFile :: Tabs -> FilePath -> IO ()
openTabFile (Tabs o n mvar paused) fp = modifyMRVar_ mvar $ \fm ->
  case FM.locateIndex fp fm of
    Just i -> do
      notebookSetCurrentPage n i -- sync point
      return (FM.setFocus i fm)
    Nothing -> do
      mb <- openFile o fp
      case mb of
        Nothing -> do
          warning ("file does not exist: " ++ show fp)
          return fm -- file does not exist; do nothing
        Just (fid, tb) -> do
          let tabname = do
                sd <- isFileSaved fid
                case sd of
                  Nothing -> return Nothing
                  Just saved -> do
                    let mod Clean = id
                        mod Dirty = ('*':)
                    allFileNames <- (map fst . FM.toAscList) `fmap` readMRVar mvar
                    return (Just (mod saved (unamb fp allFileNames)))
          rec
            let fm' = FM.insert fp (fid, editorTab) fm
            let Just i = FM.locateIndex fp fm'
            editorTab <- paused $ newHaskellTab n i tb (getMRVars mvar) tabname
            return ()
          -- sync point
          paused $ do
            notebookSetCurrentPage n i
          return (FM.setFocus i fm')

paused fs = bracket_ (modifyMVar_ fs (\_ -> return Paused)) (modifyMVar_ fs (\_ -> return Going))

-- | Returns the current tab and associated file, or Nothing if the tab collection is empty.
getCurrentTab :: Tabs -> IO (Maybe (FileId, HaskellTab))
getCurrentTab (Tabs _ _ mvar _) = do
  fm <- atomically . peekStream . readMRVar $ mvar
  case FM.lookupFocus fm of
    Nothing -> return Nothing
    Just (_fp, (fid, tb)) -> return (Just (fid, tb))

-- | Closes the currently focused tab.
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

-- | Closes all tabs (and saves all unsaved files). Called before the shutdown of the application.
closeAllTabs :: Tabs -> IO ()
closeAllTabs (Tabs _ n mvar paused) = modifyMRVar_ mvar rep where
  rep fm = foldM (\prev _ -> closeCurrentTab' n paused prev) fm [1..(FM.size fm)]

-- | Changes active tab in the notebook to the next one. Can wrap around to the front.
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

-- | Changes active tab in the notebook to the previous one. Can wrap around to the back.
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

warning = putStrLn
