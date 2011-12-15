{-# LANGUAGE
 NoMonomorphismRestriction
 #-}
module IDE.Undefineditor.Gui.View.Notebook (
  EditorTab(),
  tabWidget,
  editorTabSourceView,
  newTab,
  newSearchModuleTab
) where

import Control.Concurrent
-- import Data.Tree
import Graphics.UI.Gtk {- (
  AttrOp((:=)),
  PolicyType(PolicyNever),
  WrapMode(WrapChar),
  containerAdd,
  notebookInsertPage,
  notebookSetTabReorderable,
  scrolledWindowHscrollbarPolicy,
  scrolledWindowNew,
  set,
  textBufferSetText,
  textViewWrapMode,
  widgetShow
  ) -}
import Graphics.UI.Gtk.SourceView (
  SourceView,
  -- sourceBufferNew,
  -- sourceBufferNewWithLanguage,
  -- sourceLanguageManagerGetDefault,
  -- sourceLanguageManagerGetLanguageIds,
  -- sourceLanguageManagerGuessLanguage,
  sourceViewNewWithBuffer,
  sourceViewShowLineNumbers
  )

data EditorTab = EditorTab { tabWidget :: Widget, editorTabSourceView :: SourceView }

newTab notebook whence buffer tabName = do
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  putStrLn "created scrolled window"
  set scrolledWindow [scrolledWindowHscrollbarPolicy := PolicyNever]
  sourceView <- sourceViewNewWithBuffer buffer
  set sourceView [textViewWrapMode := WrapChar, sourceViewShowLineNumbers := True]
  putStrLn "set wrap mode"
  containerAdd scrolledWindow sourceView
  widgetShow scrolledWindow -- or do I need to show both?
  widgetShow sourceView
  putStrLn "showed the source view"
  myTid <- myThreadId
  putStrLn $ "thread id inside newTab: " ++ show myTid
  notebookInsertPage notebook scrolledWindow tabName whence
  putStrLn "insert page into notebook"
  notebookSetTabReorderable notebook scrolledWindow False
  putStrLn "made sure tabs were not reorderable"
  return (EditorTab (toWidget scrolledWindow) sourceView)

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
