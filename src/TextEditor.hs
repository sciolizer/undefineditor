module Main where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import UI.NCurses

import Square
import qualified TextWidget as TW

main = do
  [fn] <- getArgs
  contents <- readFile fn
  tw <- liftIO $ TW.newTextWidget
  liftIO $ TW.setText tw contents
  runCurses $ do
    wn <- defaultWindow
    (h,w) <- screenSize
    let twSquare = Square 0 0 (fromInteger h) (fromInteger w)
    cursorLoc <- TW.render tw twSquare
    case cursorLoc of
      Just (r,c) -> do
        setEcho True
        updateWindow wn $ moveCursorSquare twSquare r c
      Nothing -> setEcho False
    render
    waitFor wn (TW.handleEvent tw twSquare)

-- waitFor :: Window -> Curses [Event]
waitFor w twhe = loop where
  loop = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop
      Just ev' -> if ev' == EventCharacter 'q' then return () else (twhe ev' >> loop)
