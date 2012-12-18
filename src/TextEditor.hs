module Main where

import Control.Monad
import System.Environment
import UI.NCurses

main = do
  [fn] <- getArgs
  contents <- readFile fn
  runCurses $ renderFile contents

type M = Curses

renderFile contents = do
  w <- defaultWindow
  (r,c) <- screenSize
  updateWindow w $ do
    let lines = renderBuffer (fromInteger r) (fromInteger (c - 1)) contents
    forM_ (zip [0..] lines) $ \(line, (overflow, content)) -> do
      when overflow $ do
        moveCursor line 0
        drawString "\\"
      moveCursor line 1
      drawString content
  render
  waitFor w (\ev -> ev == EventCharacter 'q')

-- todo: remove rows argument; it's a lazy language after all!
renderBuffer :: Int {- ^ rows -} -> Int {- ^ columns -} -> String -> [(Bool,String)]
renderBuffer rows cols str = rb False rows str where
  rb _ 0 _ = []
  rb cont rs s =
    let lookahead = takeWhile (/= '\n') (take (cols + 1) s)
        chunk = take cols lookahead
        overflow = length lookahead > cols
        todrop = length chunk + if overflow then 0 else 1 in
    (cont, chunk) : rb overflow (rs - 1) (drop todrop s)

waitFor :: Window -> (Event -> Bool) -> Curses [Event]
waitFor w p = loop [] where
  loop evs = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop evs
      Just ev' -> if p ev' then return evs else loop (ev':evs)
