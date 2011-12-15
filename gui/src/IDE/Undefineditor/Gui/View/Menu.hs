{-# LANGUAGE
 GeneralizedNewtypeDeriving,
 RankNTypes
 #-}
module IDE.Undefineditor.Gui.View.Menu where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Graphics.UI.Gtk

import IDE.Undefineditor.Gui.Model.Activations

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

newtype MenuBuilder a = MenuBuilder (ReaderT MenuBuilderContext IO a)
  deriving (Monad, MonadIO)

data MenuBuilderContext = MenuBuilderContext {
  addChild :: forall mi. (MenuItemClass mi) => mi -> IO (),
  getAction :: Activation -> IO () }

child :: String -> Activation -> MenuBuilder ()
child label activation =
  extendMenu activation =<< liftIO (menuItemNewWithMnemonic label)

stock :: String -> Activation -> MenuBuilder ()
stock stockId activation =
  extendMenu activation =<< liftIO (imageMenuItemNewFromStock stockId)

extendMenu :: (MenuItemClass mi) => Activation -> mi -> MenuBuilder ()
extendMenu activation menuItem = MenuBuilder $ do
  MenuBuilderContext insert action <- ask
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
  local (\(MenuBuilderContext _ acts) -> MenuBuilderContext (menuShellAppend subMenu) acts) children

runMenuBuilder
  :: (MenuShellClass shell)
  => (Activation -> IO ())
  -> shell
  -> MenuBuilder ()
  -> IO ()
runMenuBuilder getAction shell (MenuBuilder readerT) = runReaderT readerT (MenuBuilderContext (menuShellAppend shell) getAction)
