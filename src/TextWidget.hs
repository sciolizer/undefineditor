{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module TextWidget where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import UI.NCurses

import Square
import TextBuffer

data TextWidget = TextWidget {
  buffer :: IORef Buffer,
  portalTop :: IORef (Int, Int), -- start of viewable portion
  fileCoord :: IORef ((Int, Int), Int {- ^ hidden column -})
  }

newTextWidget :: IO TextWidget
newTextWidget = TextWidget <$> newIORef (fromString "") <*> newIORef (0,0) <*> newIORef ((0,0), 0)

getText :: TextWidget -> IO String
getText tw = toString <$> readIORef (buffer tw)

setText :: TextWidget -> String -> IO ()
setText tw newc = writeIORef (buffer tw) (fromString newc)

render :: TextWidget -> Square -> M (Maybe (Int, Int)) {- ^ row, col for cursor, relative to square -}
render tw sq = do
  w <- defaultWindow
  buf <- liftIO $ readBuffer tw
  t <- liftIO $ readPortalTop tw
  (crsr, _) <- liftIO $ readFileCoord tw
  updateWindow w $ do
    let cols = columns sq
        wbuf = (buf, cols)
        (portalRow,_) = translate wbuf t
        ls = portalify wbuf portalRow
        scrCur = translate wbuf crsr
    forM_ (zip [0..(height sq - 1)] ls) $ \(line, (overflow, content)) -> do
      moveCursorSquare sq line 0
      drawString $ if overflow then "\\" else " "
      drawString (take (cols - 1) (content ++ repeat ' ')) -- todo: figure out why this is cols - 1 and not just cols
    return $
      let (r,c) = scrCur
          r' = r - portalRow in
      case r' of
        z | z < 0 || z >= height sq -> Nothing
          | c >= width sq - 1 -> error "splitBuffer returned invalid col"
          | otherwise -> Just (r', c + 1)

handleEvent :: TextWidget -> Square -> Event -> M ()
handleEvent tw sq e = liftIO $ do
  let move rd cd = do
        moveInternalCursor tw (columns sq) rd cd
        scrollToCursor tw sq
      modBuffer action = do
        fc <- readFileCoord tw
        cnt <- readBuffer tw
        let (cnt', fc') = action fc cnt
        writeIORef (buffer tw) cnt'
        writeIORef (fileCoord tw) fc'
      ins char = do
        modBuffer (\(f@(r,c),_) b -> (insert char f b, ((r,c),c)))
        move 0 1
      del = modBuffer (\(f@(r,c),_) b -> (fst $ delete f b, ((r,c),c)))
      newline = ins '\n'
      backspace = do
        fc <- readFileCoord tw
        case fc of
          ((0,0),_) -> return ()
          _ -> move 0 (negate 1) >> del
  case e of
    EventSpecialKey k -> do
      case k of
        KeyDownArrow -> move 1 0
        KeyUpArrow -> move (negate 1) 0
        KeyLeftArrow -> move 0 (negate 1)
        KeyRightArrow -> move 0 1
        KeyDeleteCharacter -> del
        KeyBackspace -> backspace
        KeyEnter -> newline
        KeyHome -> modifyIORef (fileCoord tw) (\((r,_),_) -> ((r, 0), 0))
        KeyEnd -> do
          buf <- readBuffer tw
          ((r,_),_) <- readFileCoord tw
          case buf `line` r of
            Nothing -> return ()
            Just l -> let ll = length l in writeIORef (fileCoord tw) ((r, ll), ll)
        _ -> return ()
    EventCharacter '\b' -> backspace
    EventCharacter '\n' -> newline
    EventCharacter c | c >= ' ' && c <= '~' -> ins c
    _ -> return ()

moveInternalCursor tw cols rd cd = z where
  z = do
    buf <- readBuffer tw
    let wb = (buf, cols)
    fc <- readFileCoord tw
    let fc' = adjust wb fc rd cd
    writeIORef (fileCoord tw) fc'
  adjust wb fc x 0 | x < 0 = adjust wb (upward wb fc) (x + 1) 0
                   | x == 0 = fc
                   | otherwise = adjust wb (downward wb fc) (x - 1) 0
  adjust wb fc x y | y < 0 = adjust wb (leftward wb fc) x (y + 1)
                   | otherwise = adjust wb (rightward wb fc) x (y - 1)

countLines tw = length . lines <$> getText tw

readFileCoord = readIORef . fileCoord
readPortalTop = readIORef . portalTop
readBuffer = readIORef . buffer

columns sq = width sq - 1

scrollToCursor tw sq = do
  let cols = columns sq
  buf <- readBuffer tw
  (fileCoord, _) <- readFileCoord tw
  t <- readPortalTop tw
  let wb = (buf, cols)
      (r,c) = translate wb fileCoord
      (pt,_) = translate wb t
  if r < pt
    then writeIORef (portalTop tw) (r,0) -- todo: replace zero with proper value
    else if r >= pt + height sq
           then {- putStrLn "scrollingDown" >> -} writeIORef (portalTop tw) (r - height sq + 1, 0) -- todo: fix zero AND fix r - height which assumes portal and file heights are the same
           else return ()
