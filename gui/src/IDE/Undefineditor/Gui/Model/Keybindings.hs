{-# LANGUAGE
 FlexibleInstances,
 NoMonomorphismRestriction,
 TypeSynonymInstances
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

import Control.Monad (when)
import Control.Monad.Trans.Writer (Writer(), execWriter, tell)
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map as M (Map(), empty, insertWithKey', lookup)
import Data.Maybe (isNothing)
import qualified Data.Set as S (Set(), empty, insert, singleton, toAscList)
import Graphics.UI.Gtk (KeyVal(), Modifier(Control, Shift), keyFromName, keyName)

import IDE.Undefineditor.Util.Reactive
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
getActivation _kb shortcut = do
  let ret = M.lookup shortcut (snd keyBindings)
  when (isNothing ret) $ do
    -- putStrLn $ "unrecognized keybinding: " ++ showKeybinding shortcut
    return ()
  return ret

-- | Looks up the shortcut associated with an activation.
getKeybinding :: Keybindings -> Activation -> Stream (Maybe Shortcut)
getKeybinding _kb act = return $ M.lookup act (fst keyBindings)

-- | A keyboard shortcut is a set of modifiers and a keyval.
type Shortcut = (S.Set Modifier, KeyVal)

keyBindings :: (M.Map Activation Shortcut, M.Map Shortcut Activation)
keyBindings = canonicalize $ do
  AFind =: (Control & "f")
  AEscape =: unmodified "Escape"
  AFindNext =: (Control & "g")
  AFindPrevious =: (Control & Shift & "G") -- interesting how I have to use capital g
  ANew =: Control & "n"
  ASaveAll =: Control & "s"
  ASelectCurrentIdentifier =: Control & "w"
  AFindOccurences =: Control & "b"

infixr 6 &
class MakesShortcut a where (&) :: Modifier -> a -> Shortcut
instance MakesShortcut String where x & y = (S.singleton x, keyFromName y)
instance MakesShortcut Shortcut where mod & (mods, k) = (S.insert mod mods, k) where
unmodified name = (S.empty, keyFromName name)

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
