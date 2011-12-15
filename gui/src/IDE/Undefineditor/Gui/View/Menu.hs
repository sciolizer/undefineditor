{-# LANGUAGE
 DoRec,
 GeneralizedNewtypeDeriving,
 RankNTypes
 #-}
module IDE.Undefineditor.Gui.View.Menu (
  MenuBuilder(),
  menuTemplate,
  runMenuBuilder
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import qualified Data.Set as S
import Graphics.UI.Gtk

import IDE.Undefineditor.Gui.Controller.Reactive
import IDE.Undefineditor.Gui.Model.Activations
import IDE.Undefineditor.Gui.Model.Keybindings

{-
newEditorMenu = do
  menuBar <- menuBarNew
  extendMenuBar menuBar

extendMenuBar menuBar = do
  fileMenu <- menuNew
  blastOff <- menuItemNewWithMnemonic "bla_st off"
  fileMenuActivator <- menuItemNewWithMnemonic "w_hee"
  menuShellAppend fileMenu blastOff
  menuItemSetSubmenu fileMenuActivator fileMenu
  menuShellAppend menuBar fileMenuActivator
  return menuBar
  -}

menuTemplate = do
  parent "_File" $ do
    stock "gtk-new" ANew
    stock "gtk-open" AOpenModule
    child "Open _File..." AOpenFile
    stock "gtk-save" ASaveAll
    stock "gtk-close" AClose
    stock "gtk-quit" AQuit
  parent "_Edit" $ do
    stock "gtk-cut" ACut
    stock "gtk-copy" ACopy
    stock "gtk-paste" APaste
    stock "gtk-find" AFind
  parent "_Run" (return ())
  parent "_Tools" $ do
    child "_Hoogle" AHoogle
    child "Rearrange _Imports" ARearrangeImports
  parent "_Tabs" $ do
    child "_Next" ATabNext
    child "_Previous" ATabPrevious
    child "To _Window" ATabToWindow
  parent "_Global" $
    -- child "_Keybindings" A
    child "_Projects" AProjects
  parent "_Context" $ do
    child "Go to _definition" AGoToDefinition
    child "Go to _usage" AGoToUsage
    child "_Find usages" AFindUsages
    child "Select current _identifier" ASelectCurrentIdentifier
    child "Find _occurences" AFindOccurences

newtype MenuBuilder a = MenuBuilder (RWST MenuBuilderContext [Activation] () IO a)
  deriving (Monad, MonadIO)

data MenuBuilderContext = MenuBuilderContext {
  addChild :: forall mi. (MenuItemClass mi) => mi -> IO (),
  getAction :: Activation -> IO (),
  getKeybindings :: Keybindings }

child :: String -> Activation -> MenuBuilder ()
child label activation = do
  kb <- MenuBuilder $ asks getKeybindings
  let name = do
        mbBind <- getKeybinding kb activation
        return (label ++ (maybe "" (\b -> " (" ++ showKeybinding b ++ ")") mbBind))
  extendMenu activation =<< liftIO (menuItemNewWithRReadMnemonic name)

menuItemNewWithRReadMnemonic :: RRead String -> IO MenuItem
menuItemNewWithRReadMnemonic str = do
  rec
    Just initialStr <- react (Just `fmap` str) $ \old new ->
      unless (old == new) $ labelSetTextWithMnemonic label new
    label <- labelNewWithMnemonic initialStr
  mi <- menuItemNew
  containerAdd mi label
  return mi

-- todo: shortcut keys are not showing up on stock items
stock :: String -> Activation -> MenuBuilder ()
stock stockId activation =
  extendMenu activation =<< liftIO (imageMenuItemNewFromStock stockId)

extendMenu :: (MenuItemClass mi) => Activation -> mi -> MenuBuilder ()
extendMenu activation menuItem = MenuBuilder $ do
  insert <- asks addChild
  action <- asks getAction
  tell [activation]
  liftIO $ do
    on menuItem menuItemActivate (action activation)
    insert menuItem

parent :: String -> MenuBuilder () -> MenuBuilder ()
parent label (MenuBuilder children) = MenuBuilder $ do
  insert <- asks addChild
  subMenu <- liftIO $ do
    parentMenuItem <- menuItemNewWithMnemonic label
    subMenu <- menuNew
    menuItemSetSubmenu parentMenuItem subMenu
    insert parentMenuItem
    return (subMenu :: Menu)
  local (\mbc -> mbc { addChild = menuShellAppend subMenu }) children

runMenuBuilder
  :: (MenuShellClass shell)
  => (Activation -> IO ())
  -> shell
  -> Keybindings
  -> MenuBuilder a
  -> IO a
runMenuBuilder getAction shell kb (MenuBuilder rwst) = do
  (ret, (), acts) <- runRWST rwst (MenuBuilderContext (menuShellAppend shell) getAction kb) ()
  let unused = S.fromList enumeration `S.difference` (S.fromList acts)
  unless (S.null unused) $ do
    putStrLn $ "Warning: activations not in menu: " ++ show (S.toAscList unused)
  return ret

enumeration :: (Enum a, Bounded a) => [a]
enumeration = [minBound..maxBound]
