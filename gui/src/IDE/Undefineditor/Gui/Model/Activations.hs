-- | A master list of all actions in the editor.
--
-- While a master list of all actions is certainly not necessary, I forsee a number of advantages.
--
-- * We get compiler warnings when we pattern select on a partial list of actions.
--
-- * Application logic can verify things like 'every action has a keyboard shortcut' and
--    'every action appears in the menu'.
--
-- * IntelliJ's 'Navigating to Action' feature can be implemented.
--
-- * Custom key-maps can be created.
module IDE.Undefineditor.Gui.Model.Activations where

-- | All of the actions in the editor.
data Activation =
    ANew -- ^ Create a new haskell module.
  | AOpenModule -- ^ Open a haskell module.
  | AOpenFile -- ^ Open a file (does not have to be haskell).
  | ASaveAll -- ^ Save all open files.
  | AClose -- ^ Close the current tab.
  | AQuit -- ^ Quit the application.
  | ACut -- ^ Cut text from currently active buffer.
  | ACopy -- ^ Copy text from currently active buffer.
  | APaste -- ^ Paste text into the currently active buffer.
  | AFind -- ^ Find a substring in the currently active buffer.
  | AHoogle -- ^ Launch hoogle.
  | ARearrangeImports -- ^ Re-arrange imports in the currently active buffer.
  | ATabNext -- ^ Switch to the next tab.
  | ATabPrevious -- ^ Switch to the previous tab.
  | ATabToWindow -- ^ Create a new window from the current tab.
  | AProjects -- ^ Configure global projects list.
  | AGoToDefinition -- ^ Go to the definition of the identifier under the cursor.
  | AGoToUsage -- ^ Go to one usage of the identifier under the cursor.
  | AFindUsages -- ^ Create a new window with all usages of the identifier under the cursor.
  | ASelectCurrentIdentifier -- ^ Highlight the current identifier.
  | AFindOccurences -- ^ Find all occurences of the identifier under the cursor. This is a temporary action, until I get 'AGoToDefinition' working properly.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
