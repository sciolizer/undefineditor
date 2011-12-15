module IDE.Undefineditor.Gui.Util.TextBuffer (
  textBufferGetContents
) where

import Graphics.UI.Gtk

textBufferGetContents :: TextBufferClass self => self -> IO String
textBufferGetContents tb = do
  start <- textBufferGetStartIter tb
  end <- textBufferGetEndIter tb
  textBufferGetText tb start end False
