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
    (h,w) <- screenSize
    let twSquare = Square 0 0 (fromInteger h) (fromInteger w)
    wn <- defaultWindow
    drawAll wn tw
    waitFor wn tw (TW.handleEvent tw twSquare)

drawAll wn tw = do
  (h,w) <- screenSize
  let twSquare = Square 0 0 (fromInteger h) (fromInteger w)
  cursorLoc <- TW.render tw twSquare
  case cursorLoc of
    Just (r,c) -> do
      setCursorMode CursorVisible
      updateWindow wn $ moveCursorSquare twSquare r c
    Nothing -> void $ setCursorMode CursorInvisible
  render
  
-- waitFor :: Window -> Curses [Event]
waitFor w tw twhe = loop where
  loop = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop
      Just ev' -> if ev' == EventCharacter 'q' then return () else (twhe ev' >> drawAll w tw >> loop)
