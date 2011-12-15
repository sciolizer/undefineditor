{-# LANGUAGE
 GeneralizedNewtypeDeriving,
 NoMonomorphismRestriction
 #-}
module IDE.Undefineditor.Gui.View.Window where

-- import Control.Concurrent.STM (atomically)
import Control.Monad
import Control.Monad.IO.Class (MonadIO(), liftIO)
import Data.Maybe
import qualified Data.Set as S
import Graphics.UI.Gtk {- (
  FileChooserAction(FileChooserActionOpen),
  Notebook(),
  ResponseId(ResponseNone,ResponseAccept,ResponseCancel,ResponseDeleteEvent),
  builderAddFromFile,
  builderGetObject,
  builderNew,
  castToMenuItem,
  castToNotebook,
  castToWindow,
  deleteEvent,
  dialogRun,
  fileChooserDialogNew,
  fileChooserGetFilename,
  initGUI,
  mainGUI,
  mainQuit,
  menuItemActivate,
  notebookRemovePage,
  notebookSetCurrentPage,
  on,
  pageReordered,
  postGUIAsync,
  switchPage,
  widgetHide,
  widgetShow,
  widgetShowAll
  ) -}
import Graphics.UI.Gtk.SourceView

import IDE.Undefineditor.Definition
import IDE.Undefineditor.Gui.Concurrent.GuiThread
import IDE.Undefineditor.Gui.Controller.Reactive
import IDE.Undefineditor.Gui.Controller.Tabs
import IDE.Undefineditor.Gui.Model.Activations
-- import qualified IDE.Undefineditor.Gui.Model.FocusedMap as FM
import IDE.Undefineditor.Gui.Model.Keybindings
import IDE.Undefineditor.Gui.Model.OpenFiles
import IDE.Undefineditor.Gui.Util.Coords
import IDE.Undefineditor.Gui.Util.ModalComboBox
import IDE.Undefineditor.Gui.Util.TextBuffer
import IDE.Undefineditor.Gui.View.Menu
import IDE.Undefineditor.Gui.View.Notebook
-- import IDE.Undefineditor.Util.Diff
import IDE.Undefineditor.Util.Safe

-- import Paths_undefineditor(getDataFileName)
getDataFileName = return

-- todo: clean shutdown... need to make sure all of the file watchers get cancelled
--
-- also todo: what happens if you attempt to close a tab which has not been saved?
-- obviously that tab should not be closed!
-- So, I guess saving a file should be done BEFORE modifying the model?
main = do
  initGUI
  postGUIAsync (putStrLn "yeah, too early")
  postGUIAsync $ do
    inGuiThread (putStrLn "does this work?")
    -- xml <- builderNew
    -- builderAddFromFile xml "hellogtk2hs.glade"
    -- Just xml <- xmlNew "hellogtk2hs.glade"
    -- notebook <- builderGetObject xml castToNotebook "notebook" :: IO Notebook
    rvars <- newRVars
    kb <- newKeybindings
    openFiles <- newOpenFiles rvars
    (windowEditor, menuBar, notebook) <- mkWindow
    tabs <- newTabs rvars openFiles notebook -- todo: we should really be able to get the rvars from the openFiles object
    {-
    tabs <- atomically $ newRVar FS.empty
    watchers <- newIORef [] -- safe because is only ever modified by the gui thread
    react (Just `fmap` readRVar tabs) $ \prevFiles curFiles -> do
      putStrLn "in responder for tabs dvar"
      let add i 
      diffAndApply (FS.toAscList prevFiles) (FS.toAscList curFiles) (notebookRemovePage notebook) (makeTab notebook)
      -- "unless" check is necessary to prevent an infinite signal firing chain
      unless (prevFiles == curFiles) $
        case FS.getFocus curFiles of
          Nothing -> return ()
          Just i -> notebookSetCurrentPage notebook i
    -- tabs <- notebookAttachmentsNew (notebook :: Notebook)
    -}
    putStrLn "wiring events"
    wireEvents notebook
    -- windowEditor <- builderGetObject xml castToWindow "windowEditor"
    -- menuBar <- builderGetObject xml castToMenuBar "menubar"
    let acts = actions windowEditor rvars openFiles tabs
    runMenuBuilder acts menuBar kb menuTemplate
    {-
    vbox <- builderGetObject xml castToVBox "vbox2"
    editorMenu <- newEditorMenu
    boxPackStart vbox editorMenu PackNatural 0
    -}
    on windowEditor deleteEvent $ liftIO $ do
      quit rvars tabs
      return True
    on windowEditor keyPressEvent $ do
      mods <- eventModifier
      kv <- eventKeyVal
      mbAct <- liftIO $ getActivation kb (S.fromList mods, kv)
      case mbAct of
        Nothing -> do
          liftIO $ putStrLn $ "ignoring " ++ show mods ++ "+" ++ keyName kv
          return False
        Just act -> do
          liftIO $ acts act
          return True
    widgetShowAll windowEditor

    -- for quick debugging:
    cleanly rvars $ openTabFile tabs "/home/jball/git/undefined/src/IDE/Undefineditor/Gui/View/Window.hs"
    -- buildAll ac

  mainGUI

-- todo: we should really be able to get the rvars from the tabs object directly
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
  -- boxPackStart vbox findBox PackNatural 0
  containerAdd window vbox
  return (window, menuBar, notebook)

mkFindBox = do
  findBox <- hBoxNew False 0
  entry <- entryNew
  prev <- buttonNewWithLabel "Prev"
  next <- buttonNewWithLabel "Next"
  boxPackStart findBox entry PackGrow 0
  boxPackStart findBox prev PackNatural 0
  boxPackStart findBox next PackNatural 0
  return findBox

data Tab = TabFile String | TabOpenModule
  deriving (Eq, Ord) -- todo: replace Ord instance with a better one

actions window rvars openFiles tabs activation =
  case activation of
    ANew -> return ()
    AOpenModule -> return () -- change tabs (FS.focusOn TabOpenModule . FS.insert TabOpenModule) -- todo: highlight contents of entry box
    AOpenFile -> do
      filename <- fileDialog
      maybe (return ()) (cleanly rvars . openTabFile tabs) filename
    ASaveAll -> cleanly rvars $ saveDirtyFiles openFiles
    AClose -> cleanly rvars $ closeCurrentTab tabs
    AQuit -> quit rvars tabs
    ACut -> return ()
    ACopy -> return ()
    APaste -> return ()
    AFind -> return () -- textIterForwardSearch and textIterBackwardSearch will probably be useful functions for this
    AHoogle -> return ()
    ARearrangeImports -> return ()
    ATabNext -> cleanly rvars $ nextTab tabs -- change tabs (modFocus 1)
    ATabPrevious -> cleanly rvars $ previousTab tabs -- change tabs (modFocus (-1))
    ATabToWindow -> return ()
  --   AKeybindings leaving out because should not have any keybindings -> return ()
    AProjects -> return ()
    AGoToDefinition -> putStrLn "go to definition not implemented"
    AGoToUsage -> return ()
    AFindUsages -> return ()
    ASelectCurrentIdentifier -> withCurrentTab tabs $ \_fid sourceView -> do
      tb <- textViewGetBuffer sourceView
      contents <- textBufferGetContents tb
      co <- textBufferGetInsertOffset tb
      mbLexeme <- getLexeme contents co
      case mbLexeme of
        Nothing -> putStrLn "no identifier underneath cursor"
        Just (_, start, limit) -> do
          [stIter, limIter] <- mapM (textBufferGetIterAtOffset tb) [start, limit]
          textBufferSelectRange tb stIter limIter
    AFindOccurences -> withCurrentTab tabs $ \fid sourceView -> do
      tb <- textViewGetBuffer sourceView
      contents <- textBufferGetContents tb
      co <- textBufferGetInsertOffset tb
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
          putStrLn $ "buffer coords: " ++ show (x, y)
          mapM_ (\twt -> putStrLn . ((show twt ++ ": ") ++) . show =<< textViewBufferToWindowCoords sourceView twt (x, y)) [TextWindowWidget .. TextWindowBottom]
          (x', y') <- textViewBufferToWindowCoords sourceView {- TextWindowWidget {- TextWindowText, -} -} TextWindowLeft (x,y)
          putStrLn $ "window coords: " ++ show (x', y')
          (x'', y'') <- windowToScreenCoords window (x', y')
          putStrLn $ "screen coords: " ++ show (x'', y'')
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
  textViewScrollToMark sourceView insertMark 0.0 (Just (0.5, 0.5)) -- put insert mark in center

textBufferGetInsertOffset tb =
  textIterGetOffset =<< textBufferGetIterAtMark tb =<< textBufferGetInsert tb

withCurrentTab :: Tabs -> (FileId -> SourceView -> IO ()) -> IO ()
withCurrentTab tabs act = do
  mbTb <- getCurrentTab tabs
  case mbTb of
    Nothing -> return ()
    Just (fid, et) -> act fid (editorTabSourceView et)

-- wireEvents :: NotebookAttachments TabContext -> IO ()
wireEvents notebook = do
  {-
  let forMenuItem s act = do
        entry <- builderGetObject xml castToMenuItem s
        on entry menuItemActivate (safe act)
        -}
  -- notebook <- notebookAttachmentsGetNotebook tabs
  on notebook pageReordered (\_ i -> safe (putStrLn $ "page reordered: " ++ show i)) -- I think reordering is disabled, so this should never fire.
  -- on notebook switchPage (\i -> safe (putStrLn ("switched page: " ++ show i) >> change tabs (FS.setFocus i)))
  -- on notebook switchPage (\i -> safe (showSwitchedPage tabs notebook i)) -- todo: restore functionality
  -- on menuItemNewTab menuItemActivate (safe (newTab notebook))
  return ()

{-
change v f = cleanly $ do
  x <- liftReadM (readRVar v)
  writeRVar v (f x)
  -}

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
  widgetHide dialog -- eh? not destroy? leak, maybe?
  return ret

{-
loadFile tabs f =
  change tabs (FS.focusOn t . FS.insert t) where
    t = TabFile f
    -}

{-
goToDefinition sourceView = do
  textBuffer <- textViewGetBuffer sourceView
  textMark <- textBufferGetInsert textBuffer
  textIter <- textBufferGetIterAtMark textBuffer textMark
  i <- textIterGetOffset textIter
  putStrLn ("Cursor position is: " ++ show i)
-}

{-
buildAll ac = do
  let nb = notebook . acWidgets $ ac
  num <- notebookGetNPages nb
  let getFp i = tcFilePath . fromJust <$> (notebookGetAttachment (acTabContexts ac) . fromJust =<< notebookGetNthPage nb i)
  fps <- mapM getFp [0..(num-1)]
  print fps
  -- runUndefined ac (build fps)
  build (head fps) -- todo: shouldn't be head
  -}

-- todo: this function needs an additional argument
buildCurrent = putStrLn "building current"

{-
showSwitchedPage tabs nb i = do
  (Just w) <- notebookGetNthPage nb i
  (Just (TabContext fp)) <- notebookAttachmentsGet tabs w
  putStrLn $ "Filepath is: " ++ fp
  -}

internalBug = error
