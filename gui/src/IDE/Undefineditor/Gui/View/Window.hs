{-# LANGUAGE
 GeneralizedNewtypeDeriving,
 NoMonomorphismRestriction
 #-}

-- | Currently the main entry point for the application.
-- This will likely change in the future, when I support
-- having multiple windows.
module IDE.Undefineditor.Gui.View.Window (
  main
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO(), liftIO)
import qualified Data.Set as S (fromList)
import Graphics.UI.Gtk (
  FileChooserAction(FileChooserActionOpen),
  Packing(PackGrow, PackNatural),
  ResponseId(ResponseNone,ResponseAccept,ResponseCancel,ResponseDeleteEvent),
  boxPackStart,
  containerAdd,
  deleteEvent,
  dialogRun,
  eventKeyVal,
  eventModifier,
  fileChooserDialogNew,
  fileChooserGetFilename,
  initGUI,
  keyPressEvent,
  mainGUI,
  mainQuit,
  menuBarNew,
  notebookNew,
  on,
  postGUIAsync,
  textBufferGetInsert,
  textBufferGetIterAtLine,
  textBufferGetIterAtMark,
  textBufferGetIterAtOffset,
  textBufferPlaceCursor,
  textBufferSelectRange,
  textIterBackwardChars,
  textIterCopy,
  textIterForwardChars,
  textIterGetLine,
  textIterGetText,
  textIterGetOffset,
  textIterGetVisibleLineOffset,
  textIterOrder,
  textViewGetBuffer,
  textViewScrollToMark,
  vBoxNew,
  widgetHide,
  widgetShow,
  widgetShowAll,
  windowNew
  )
import Graphics.UI.Gtk.SourceView (SourceView())

import IDE.Undefineditor.Definition
import IDE.Undefineditor.Util.Reactive
import IDE.Undefineditor.Gui.Controller.Tabs
import IDE.Undefineditor.Gui.Model.Activations
import IDE.Undefineditor.Gui.Model.Keybindings
import IDE.Undefineditor.Gui.Model.OpenFiles
import IDE.Undefineditor.Gui.Util.ModalComboBox
import IDE.Undefineditor.Gui.Util.TextBuffer
import IDE.Undefineditor.Gui.View.FindBar
import IDE.Undefineditor.Gui.View.Menu
import IDE.Undefineditor.Gui.View.Notebook

-- | Initializes gtk and creates a new window.
main :: IO ()
main = do
  initGUI
  postGUIAsync $ do
    rvars <- newRVars
    kb <- newKeybindings
    openFiles <- newOpenFiles rvars
    (windowEditor, menuBar, notebook) <- mkWindow
    tabs <- newTabs rvars openFiles notebook
    {-
    tabs <- atomically $ newRVar FS.empty
    watchers <- newIORef [] -- safe because is only ever modified by the gui thread
    react (Just `fmap` readRVar tabs) $ \prevFiles curFiles -> do
      let add i 
      diffAndApply (FS.toAscList prevFiles) (FS.toAscList curFiles) (notebookRemovePage notebook) (makeTab notebook)
      -- "unless" check is necessary to prevent an infinite signal firing chain
      unless (prevFiles == curFiles) $
        case FS.getFocus curFiles of
          Nothing -> return ()
          Just i -> notebookSetCurrentPage notebook i
    -- tabs <- notebookAttachmentsNew (notebook :: Notebook)
    -}
    let acts = actions windowEditor rvars openFiles tabs
    buildMenu menuBar acts kb
    on windowEditor deleteEvent $ liftIO $ do
      quit rvars tabs
      return True
    on windowEditor keyPressEvent $ do
      mods <- eventModifier
      kv <- eventKeyVal
      mbAct <- liftIO $ getActivation kb (S.fromList mods, kv)
      case mbAct of
        Nothing -> do
          return False
        Just act -> do
          liftIO $ acts act
          return True
    widgetShowAll windowEditor
  mainGUI

quit rvars tabs = do
  cleanly rvars $ closeAllTabs tabs -- saves all files
  mainQuit

mkWindow = do
  window <- windowNew
  vbox <- vBoxNew False 0
  menuBar <- menuBarNew
  notebook <- notebookNew
  boxPackStart vbox menuBar PackNatural 0
  boxPackStart vbox notebook PackGrow 0
  containerAdd window vbox
  return (window, menuBar, notebook)

data Tab = TabFile String | TabOpenModule
  deriving (Eq, Ord) -- todo: replace Ord instance with a better one

actions window rvars openFiles tabs activation = ret where
  getBufferContentsAndOffset sourceView = do
      tb <- textViewGetBuffer sourceView
      contents <- textBufferGetContents tb
      co <- textBufferGetInsertOffset tb
      return (tb, contents, co)
  ret = case activation of
    ANew -> return ()
    AOpenModule -> return () -- todo: highlight contents of entry box
    AOpenFile -> do
      filename <- fileDialog
      maybe (return ()) (cleanly rvars . openTabFile tabs) filename
    ASaveAll -> cleanly rvars $ saveDirtyFiles openFiles
    AClose -> cleanly rvars $ closeCurrentTab tabs
    AQuit -> quit rvars tabs
    ACut -> return ()
    ACopy -> return ()
    APaste -> return ()
    AEscape -> withCurrentTab tabs $ \fid ht -> hideFindBar (haskellTabFindBar ht)
    AFind -> withCurrentTab tabs $ \fid ht -> focusFindBar (haskellTabFindBar ht)
    AFindNext -> withCurrentTab tabs $ \fid ht -> findNext (haskellTabFindBar ht)
    AFindPrevious -> withCurrentTab tabs $ \fid ht -> findPrevious (haskellTabFindBar ht)
    AHoogle -> return ()
    ARearrangeImports -> return ()
    ATabNext -> cleanly rvars $ nextTab tabs
    ATabPrevious -> cleanly rvars $ previousTab tabs
    ATabToWindow -> return ()
    AProjects -> return ()
    AGoToDefinition -> putStrLn "go to definition not implemented"
    AGoToUsage -> return ()
    AFindUsages -> return ()
    ASelectCurrentIdentifier -> withCurrentTab tabs $ \_fid haskellTab -> do
      (tb, contents, co) <- getBufferContentsAndOffset (haskellTabSourceView haskellTab)
      mbLexeme <- getLexeme contents co
      case mbLexeme of
        Nothing -> putStrLn "no identifier underneath cursor"
        Just (_, start, limit) -> do
          [stIter, limIter] <- mapM (textBufferGetIterAtOffset tb) [start, limit]
          textBufferSelectRange tb stIter limIter
    AFindOccurences -> withCurrentTab tabs $ \fid haskellTab -> do
      let sourceView = haskellTabSourceView haskellTab
      (tb, contents, co) <- getBufferContentsAndOffset sourceView
      mbLocs <- definitionCandidates (getFilePath fid) contents co
      case mbLocs of
        Nothing -> return ()
        Just [] -> return ()
        -- todo: don't ignore the FilePath
        Just [(_, offset)] -> jumpTo offset sourceView tb
        Just offsets -> do
          {-
        textViewGetLineYrange :: self -> TextIter -> IO (Int, Int)
        textViewGetIterLocation :: TextViewClass self => self -> TextIter -> IO Rectangle
        data Rectangle x y width height
        textViewBufferToWindowCoords :: self -> TextWindowType -> (Int, Int) -> IO (Int, Int)
        drawWindowGetOrigin???
        windowGetFrameDimensions :: self -> IO (Int, Int, Int, Int) -- unfortunately this seems to involve doing the arithmetic myself
        textViewGetLineAtY :: self -> Int -> IO (
          insertMark <- textBufferGetInsert tb
          textViewScrollToMark sourceView insertMark 0.0 Nothing
          iter <- textBufferGetIterAtMark tb insertMark
          Rectangle x y width height <- textViewGetIterLocation sourceView iter
          (x', y') <- textViewBufferToWindowCoords sourceView {- TextWindowWidget {- TextWindowText, -} -} TextWindowLeft (x,y)
          (x'', y'') <- windowToScreenCoords window (x', y')
        -}
          actions <- forM offsets $ \(_,o) -> do -- todo: don't ignore the FilePath
            iter <- textBufferGetIterAtOffset tb o
            line <- textIterGetLine iter -- +1 is for 1-based indexing
            col <- textIterGetVisibleLineOffset iter
            beginIter <- textBufferGetIterAtLine tb line
            endIter <- textBufferGetIterAtLine tb (line + 1)
            backForty <- textIterCopy iter
            -- todo: this 40 character thing is still kind of janky.... e.g. if the found word is at the front of a large line, you will only end up showing 40 characters, whereas if the found word is near the middle of a large line, you will show 80 characters.
            textIterBackwardChars backForty 40
            forwForty <- textIterCopy iter
            textIterForwardChars forwForty 40
            textIterOrder backForty beginIter
            textIterOrder endIter forwForty
            text <- textIterGetText beginIter endIter
            return (show (line + 1) ++ ", " ++ show (col + 1) ++ " " ++ text, jumpTo o sourceView tb)
          runModalComboBox window {- GravityNorthAest x'' y'' -} actions

jumpTo offset sourceView tb = do
  textBufferPlaceCursor tb =<< textBufferGetIterAtOffset tb offset
  insertMark <- textBufferGetInsert tb
  textViewScrollToMark sourceView insertMark 0.0 (Just (0.5, 0.5)) -- puts insert mark in center of screen

textBufferGetInsertOffset tb =
  textIterGetOffset =<< textBufferGetIterAtMark tb =<< textBufferGetInsert tb

withCurrentTab :: Tabs -> (FileId -> HaskellTab -> IO ()) -> IO ()
withCurrentTab tabs act = do
  mbTb <- getCurrentTab tabs
  case mbTb of
    Nothing -> return ()
    Just (fid, tab) -> act fid tab

fileDialog = do
  dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [("gtk-open", ResponseAccept), ("gtk-cancel", ResponseCancel)]
  widgetShow dialog
  response <- dialogRun dialog
  ret <- case response of
    ResponseAccept -> fileChooserGetFilename dialog
    ResponseCancel -> return Nothing
    ResponseDeleteEvent -> return Nothing
    ResponseNone -> internalBug "got ResponseNone from file open dialog"
    _ -> internalBug "got unexpected response code from file open dialog"
  widgetHide dialog
  return ret

internalBug = error
