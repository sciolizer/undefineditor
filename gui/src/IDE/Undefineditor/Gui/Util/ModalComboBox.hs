-- | A dialog box presenting the user with a collection of options.
module IDE.Undefineditor.Gui.Util.ModalComboBox (
  runModalComboBox
) where

import Control.Monad (unless, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Graphics.UI.Gtk (
  Packing(PackGrow),
  ResponseId(ResponseCancel, ResponseNone, ResponseOk),
  WindowClass(),
  boxPackStart,
  changed,
  comboBoxAppendText,
  comboBoxGetActive,
  comboBoxNewText,
  comboBoxPopup,
  dialogAddButton,
  dialogGetUpper,
  dialogNew,
  dialogResponse,
  mainIteration,
  mainQuit,
  on,
  response,
  widgetHide,
  widgetShow,
  windowSetModal,
  windowSetTransientFor
  )

-- | Presents the user with a collection of options, and executes the action associated
-- with that option.
runModalComboBox
  :: WindowClass parent
  => parent -- ^ The parent window. This is used to center the new dialog box.
  -> [(String, IO ())] -- ^ A list of options to present to the user, with their corresponding actions.
  -> IO () -- ^ The action selected by the user, or nothing if the user opted to cancel.
runModalComboBox _ [] = return ()
runModalComboBox parentWindow options = do
  dialog <- dialogNew
  comboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText comboBox . fst) options
  vbox <- dialogGetUpper dialog
  boxPackStart vbox comboBox PackGrow 0
  widgetShow comboBox
  dialogAddButton dialog "gtk-cancel" ResponseCancel
  windowSetTransientFor dialog parentWindow
  windowSetModal dialog True
  ref <- newIORef False
  ridRef <- newIORef ResponseNone
  on dialog response $ \rid -> do
    writeIORef ref True
    writeIORef ridRef rid
  on comboBox changed $ dialogResponse dialog ResponseOk
  widgetShow dialog
  comboBoxPopup comboBox
  waitForSelection ref
  rid <- readIORef ridRef
  -- rid <- dialogRun dialog

  widgetHide dialog -- todo: this should be in the "finally" of a bracket
  when (rid == ResponseOk) $ do
    which <- comboBoxGetActive comboBox
    unless (which == (-1)) $ snd (options !! which)

-- todo: put some brackets in here
-- also todo: I don't think a comboBox is the right control for this;
--   I think a TreeView would be more appropriate (though the hierarchy will
--   be flat)... just going with combox for now because it's simpler
-- also todo: support for the escape key... actually anything that makes the popup go away is probably fine for terminating the mainIteration
-- runModalComboBox :: Gravity -> Int -> Int -> [(String, IO ())] -> IO ()
{-
runModalComboBox _ [] = return ()
runModalComboBox parentWindow {- gravity x y -} options = do
  window <- windowNewPopup
  ref <- newIORef False
  comboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText comboBox) (map fst options)
  -- I don't think "changed" is right either, because what if you want to stick with the same one?
  on comboBox changed (writeIORef ref True)
  on comboBox focusOutEvent (liftIO $ writeIORef ref True >> return True)
  {- popupShownNotify seems to have a bug in it
  on comboBox popupShownNotify $ do
    b <- get comboBox comboBoxPopupShown
    unless b $ writeIORef ref True
    -}
  containerAdd window comboBox
  widgetShow comboBox
  set window [windowModal := True] -- todo: this modality constraint is not actually working... do I need to set a parent window?
  -- windowSetGravity window gravity
  windowSetDecorated window False
  windowSetTypeHint window WindowTypeHintPopupMenu -- WindowTypeHintDropdownMenu -- also PopupMenu
  windowSetTransientFor window parentWindow
  windowSetPosition window WinPosCenterOnParent -- WinPosCenter
  widgetShow window
  -- windowMove window x y
  comboBoxPopup comboBox
  waitForSelection ref
  widgetHide window
  which <- comboBoxGetActive comboBox
  unless (which == (-1)) $ do
    snd (options !! which)

      -}
waitForSelection ref = do
  q <- mainIteration
  if q
    then mainQuit
    else do
      b <- readIORef ref
      unless b $ waitForSelection ref
