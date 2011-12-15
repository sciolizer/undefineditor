module IDE.Undefineditor.Gui.Model.Keybindings (
  Keybindings(),
  getKeybindings,
  getActivation
) where

import Graphics.UI.Gtk

import IDE.Undefineditor.Gui.Model.Activations

data Keybindings = Keybindings

getKeybindings :: IO Keybindings
getKeybindings = return Keybindings

getActivation :: Keybindings -> [Modifier] -> KeyVal -> IO (Maybe Activation)
getActivation _kb mods kv = return ret where -- do
  ret =
    case mods of
      [Control] ->
        case keyName kv of
          "f" -> Just AFind
          "n" -> Just ANew
          "s" -> Just ASaveAll
          "w" -> Just ASelectCurrentIdentifier
          "b" -> Just AFindOccurences
          _ -> Nothing
      _ -> Nothing
