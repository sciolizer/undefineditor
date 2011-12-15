{-# LANGUAGE
 NoMonomorphismRestriction
 #-}
module IDE.Undefineditor.Gui.Model.Keybindings (
  Keybindings(),
  Shortcut,
  newKeybindings,
  getActivation,
  getKeybinding,
  showKeybinding
) where

import Control.Monad.Trans.Writer
import Data.Function
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Graphics.UI.Gtk hiding (on)

import IDE.Undefineditor.Gui.Controller.Reactive
import IDE.Undefineditor.Gui.Model.Activations

-- who creates an Enum instance without creating an Ord instance???
instance Ord Modifier where compare = compare `on` fromEnum

data Keybindings = Keybindings

newKeybindings :: IO Keybindings
newKeybindings = return Keybindings

getActivation :: Keybindings -> Shortcut -> IO (Maybe Activation)
getActivation _kb shortcut = return $ M.lookup shortcut (snd keyBindings)

getKeybinding :: Keybindings -> Activation -> RRead (Maybe Shortcut)
getKeybinding _kb act = return $ M.lookup act (fst keyBindings)

type Shortcut = (S.Set Modifier, KeyVal)

keyBindings :: (M.Map Activation Shortcut, M.Map Shortcut Activation)
keyBindings = canonicalize $ do
  AFind =: (Control & "f")
  ANew =: Control & "n"
  ASaveAll =: Control & "s"
  ASelectCurrentIdentifier =: Control & "w"
  AFindOccurences =: Control & "b"

infixl 6 &
x & y = (S.singleton x, keyFromName y)

infixl 5 =:
(=:) :: Activation -> Shortcut -> Writer [(Activation, Shortcut)] ()
act =: shortcut = tell [(act, shortcut)]

canonicalize :: Writer [(Activation, Shortcut)] () -> (M.Map Activation Shortcut, M.Map Shortcut Activation)
canonicalize m = (left, right) where
  bs :: [(Activation, Shortcut)]
  bs = execWriter m
  left = foldr (\(k,v) -> inject k v) M.empty bs
  right = foldr (\(k,v) -> inject v k) M.empty bs

inject = M.insertWithKey' (\k _ -> error $ "duplicate: " ++ show k)

showKeybinding :: Shortcut -> String
showKeybinding (mods, kv) = intercalate "+" (map show (S.toAscList mods)) ++ "+" ++ keyName kv
