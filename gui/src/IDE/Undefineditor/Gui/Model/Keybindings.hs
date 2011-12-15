{-# LANGUAGE
 NoMonomorphismRestriction
 #-}

-- | Manages keybindings.
module IDE.Undefineditor.Gui.Model.Keybindings (
  Keybindings(),
  Shortcut,
  newKeybindings,
  getActivation,
  getKeybinding,
  showKeybinding
) where

import Control.Monad.Trans.Writer (Writer(), execWriter, tell)
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map as M (Map(), empty, insertWithKey', lookup)
import qualified Data.Set as S (Set(), singleton, toAscList)
import Graphics.UI.Gtk (KeyVal(), Modifier(Control), keyFromName, keyName)

import IDE.Undefineditor.Gui.Controller.Reactive
import IDE.Undefineditor.Gui.Model.Activations

-- who creates an Enum instance without creating an Ord instance???
instance Ord Modifier where compare = compare `on` fromEnum

-- | A bidirectional map between keyboard shortcuts and 'Activation's. 
data Keybindings = Keybindings -- current implementation of keybindings is static, but the interface is written to allow keybindings to be dynamically changed

-- | Instantiates a map.
newKeybindings :: IO Keybindings
newKeybindings = return Keybindings

-- | Looks up the activation associated with a keyboard shortcut.
getActivation :: Keybindings -> Shortcut -> IO (Maybe Activation)
getActivation _kb shortcut = return $ M.lookup shortcut (snd keyBindings)

-- | Looks up the shortcut associated with an activation.
getKeybinding :: Keybindings -> Activation -> Stream (Maybe Shortcut)
getKeybinding _kb act = return $ M.lookup act (fst keyBindings)

-- | A keyboard shortcut is a set of modifiers and a keyval.
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

-- | Human readable keyboard shortcut.
showKeybinding :: Shortcut -> String
showKeybinding (mods, kv) = intercalate "+" (map show (S.toAscList mods)) ++ "+" ++ keyName kv
