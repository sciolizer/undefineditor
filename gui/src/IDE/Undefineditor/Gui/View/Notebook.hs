{-# LANGUAGE
 NoMonomorphismRestriction
 #-}

-- | Convenience functions for dealing with Gtk 'Notebook's.
module IDE.Undefineditor.Gui.View.Notebook (
  HaskellTab(),
  newHaskellTab,
  haskellTabSourceView,
  haskellTabFindBar
  -- newSearchModuleTab
) where

import Control.Monad (unless)
import Graphics.UI.Gtk (
  AttrOp((:=)),
  NotebookClass(),
  Packing(PackGrow, PackNatural),
  PolicyType(PolicyNever),
  WrapMode(WrapChar),
  boxPackStart,
  containerAdd,
  notebookInsertPage,
  notebookPageNum,
  notebookRemovePage,
  notebookSetTabLabelText,
  notebookSetTabReorderable,
  scrolledWindowHscrollbarPolicy,
  scrolledWindowNew,
  set,
  textViewWrapMode,
  toTextView,
  vBoxNew,
  widgetHide,
  widgetShow
  )
import Graphics.UI.Gtk.SourceView (
  SourceBuffer(),
  SourceView(),
  sourceViewNewWithBuffer,
  sourceViewShowLineNumbers
  )

import IDE.Undefineditor.Util.Reactive
import IDE.Undefineditor.Gui.View.FindBar

-- | Haskell tabs are tabs for editing *.hs and *.lhs files. They consist of a source view and a find bar.
data HaskellTab = HaskellTab {
  -- | Returns the 'SourceView' used for editing the contents of the file.
  haskellTabSourceView :: SourceView,
  -- | Returns the 'FindBar' appearing at the bottom of the tab page contents.
  haskellTabFindBar :: FindBar }

-- | Constructs a new tab for editing haskell.
newHaskellTab
  :: NotebookClass self
  => self -- ^ 'Notebook' to add tab to.
  -> Int -- ^ Index at which to insert tab.
  -> SourceBuffer -- ^ Editor to put in body of notebook page.
  -> RVars
  -> Stream (Maybe String) -- ^ Name of the tab. 'Nothing' indicates file has been deleted or closed.
  -> IO HaskellTab
newHaskellTab notebook whence buffer rvars tabName = do
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  set scrolledWindow [scrolledWindowHscrollbarPolicy := PolicyNever]
  sourceView <- sourceViewNewWithBuffer buffer
  set sourceView [textViewWrapMode := WrapChar, sourceViewShowLineNumbers := True]
  containerAdd scrolledWindow sourceView
  widgetShow scrolledWindow -- still haven't figured when widgetShow is and is not necessary
  widgetShow sourceView
  vbox <- vBoxNew False 0
  tabName <- registerNameUpdater rvars notebook vbox tabName
  fb <- newFindBar rvars (toTextView sourceView)
  widgetHide (findBarWidget fb)
  case tabName of
    Nothing -> return (HaskellTab sourceView fb) -- file was closed or deleted; don't insert tab into notebook
    Just str -> do
      boxPackStart vbox scrolledWindow PackGrow 0
      boxPackStart vbox (findBarWidget fb) PackNatural 0
      notebookInsertPage notebook vbox str whence
      notebookSetTabReorderable notebook vbox False
      widgetShow vbox
      return (HaskellTab sourceView fb)

data UpdateAction = SetText String | CloseTab
  deriving (Eq, Ord)

registerNameUpdater rvars notebook widget name = do
  finished <- newRVarIO rvars False
  let reactee = do
        fin <- readRVar finished
        if fin then return Nothing else (Just . maybe CloseTab SetText) `fmap` name
  ret <- reactIO reactee $ \o n -> unless (o == n) $
    case n of
      SetText s -> notebookSetTabLabelText notebook widget s
      CloseTab -> do
        mbNum <- notebookPageNum notebook widget
        case mbNum of
          Nothing -> putStrLn "todo: trying to remove widget from notebook more than once; this probably indicates a space leak"
          Just i -> notebookRemovePage notebook i
        cleanlyWriteRVar finished True -- unregister handler so that widget can be garbage collected
  case ret of
    Nothing -> return Nothing
    Just CloseTab -> return Nothing
    Just (SetText s) -> return (Just s)

{-
newSearchModuleTab notebook = do
  vbox <- vBoxNew False 0
  entry <- entryNew
  treeStore <- listStoreNew ["one", "two"]
  -- todo: treeview needs to expand to fill its space
  -- todo: also, treeview contents (one and two) are not showing up for some reason
  treeView <- treeViewNewWithModel treeStore
  treeViewSetHeadersVisible treeView False

  -- there should be a simpler way to render a list as the following!
  col <- treeViewColumnNew
  treeViewColumnSetTitle col "english number"
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer False -- don't know what this line does.
  -- amazingly, this next line seems to suggest that a single treeview can
  -- have multiple columns which aren't necessarily using the same store!
  cellLayoutSetAttributes col renderer treeStore $ \ind -> [cellText := ind]
  treeViewAppendColumn treeView col

  boxPackStart vbox entry PackNatural 0
  boxPackStart vbox treeView PackGrow 0
  widgetShow vbox
  widgetShow entry
  widgetShow treeView
  notebookAppendPage notebook vbox "Open Module"
  notebookSetTabReorderable notebook vbox False -- what? Why does this not take an index?
  -}
