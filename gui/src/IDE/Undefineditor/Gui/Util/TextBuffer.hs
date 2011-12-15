-- | Convenience functions for working with 'TextBuffer's.
module IDE.Undefineditor.Gui.Util.TextBuffer (
  textBufferGetContents
) where

import Graphics.UI.Gtk (
  TextBufferClass(),
  textBufferGetEndIter,
  textBufferGetStartIter,
  textBufferGetText)

-- | Gets the contents of the given 'TextBuffer'.
textBufferGetContents :: TextBufferClass self => self -> IO String
textBufferGetContents tb = do
  start <- textBufferGetStartIter tb
  end <- textBufferGetEndIter tb
  textBufferGetText tb start end False
