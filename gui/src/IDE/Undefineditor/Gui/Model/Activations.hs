module IDE.Undefineditor.Gui.Model.Activations where

data Activation =
    ANew
  | AOpenModule
  | AOpenFile
  | ASaveAll
  | AClose
  | AQuit
  | ACut
  | ACopy
  | APaste
  | AFind
  | AHoogle
  | ARearrangeImports
  | ATabNext
  | ATabPrevious
  | ATabToWindow
  -- | AKeybindings leaving out because should not have any keybindings
  | AProjects
  | AGoToDefinition
  | AGoToUsage
  | AFindUsages
  | ASelectCurrentIdentifier
  | AFindOccurences
