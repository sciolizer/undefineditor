module Square where

import UI.NCurses

data Square = Square { top :: Int, left :: Int, height :: Int, width :: Int }

type M = Curses

moveCursorSquare sq r c = moveCursor (toInteger $ r + top sq) (toInteger $ c + left sq)
