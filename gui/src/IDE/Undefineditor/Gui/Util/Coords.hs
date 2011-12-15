module IDE.Undefineditor.Gui.Util.Coords where

import Graphics.UI.Gtk

windowToScreenCoords :: WindowClass self => self -> (Int, Int) -> IO (Int, Int)
windowToScreenCoords window (x, y) = do
  -- todo: put frame border width calculations in here as well
  b@(xd, yd) <- windowGetPosition window
  putStrLn $ "window position is: " ++ show b
  return (x + xd, y + yd)
