{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import UI.NCurses

-- canDefineColor returns true when
-- you install rxvt-unicode-256color
-- and run the program inside rxvt.

{-
main = do
  x <- runCurses canDefineColor
  print x

import UI.NCurses
-}

main :: IO ()
main = (mapM_ print =<<) $ runCurses $ do
  setEcho False
  w <- defaultWindow
{-
  updateWindow w $ do
    moveCursor 1 10
    drawString "Hello world!"
    moveCursor 3 10
    drawString "(press q to quit)"
    moveCursor 2 10
    drawString "\955\948\219"
    moveCursor 0 0
  mapM (paint w 5 0) [(x,y) | x <- [minBound..maxBound], y <- [minBound..maxBound]]
  updateWindow w $ setColor defaultColorID
  mapM (attr w 13 0) [minBound..maxBound]
  updateWindow w $ do
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  render
  setCBreak True
  waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
  setCursorMode CursorInvisible
  beep
  waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
  setCursorMode CursorVeryVisible
  flash
  -}
  evs <- waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
  -- mapM queryColor [minBound..maxBound]
  -- maxColorID
  return evs
  -- baudrate

attr w r c a = do
  let g = Glyph 'u' [a]
  updateWindow w $ do
    moveCursor (r + 2 * (fromIntegral $ fromEnum a)) c
    drawLineH (Just g) 20
    moveCursor (r + 1 + 2 * (fromIntegral $ fromEnum a)) c
    drawString (show a)

paint w r c (f,b) = when (fromEnum f > 0 || fromEnum b > 0) $ do
  cid <- newColorID f b (fromIntegral (fromEnum f * 8 + fromEnum b))
  updateWindow w $ do
    moveCursor (r + (fromIntegral $ fromEnum f)) (c + (fromIntegral $ fromEnum b))
    setColor cid
    drawString "x"

deriving instance Bounded Color
deriving instance Enum Color

deriving instance Bounded Attribute
deriving instance Enum Attribute

waitFor :: Window -> (Event -> Bool) -> Curses [Event]
waitFor w p = loop [] where
  loop evs = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> liftIO (putStrLn "Nothing") >> loop evs
      Just ev' -> if p ev' then return evs else liftIO (print ev') >> loop (ev':evs)
