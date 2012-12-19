{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module TextWidget where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS
import Data.IORef
import UI.NCurses

import Square
import TextBuffer

data TextWidget = TextWidget {
  buffer :: IORef Buffer,
  portalTop :: IORef (Int, Int), -- start of viewable portion
  fileCoord :: IORef (Int, Int)
  }

newTextWidget :: IO TextWidget
newTextWidget = TextWidget <$> newIORef (fromString "") <*> newIORef (0,0) <*> newIORef (0,0)

getText :: TextWidget -> IO String
getText tw = toString <$> readIORef (buffer tw)

setText :: TextWidget -> String -> IO ()
setText tw newc = writeIORef (buffer tw) (fromString newc)

render :: TextWidget -> Square -> M (Maybe (Int, Int)) {- ^ row, col for cursor, relative to square -}
render tw sq = do
  w <- defaultWindow
  buf <- liftIO $ readBuffer tw
  t <- liftIO $ readPortalTop tw
  crsr <- liftIO $ readFileCoord tw
  updateWindow w $ do
    let wbuf = (buf, columns sq)
        (portalRow,_) = translate wbuf t
        ls = portalify wbuf portalRow
        scrCur = translate wbuf crsr
    forM_ (zip [0..(height sq - 1)] ls) $ \(line, (overflow, content)) -> do
      when overflow $ do
        moveCursorSquare sq line 0
        drawString "\\"
      moveCursorSquare sq line 1
      drawString content
    return $
      let (r,c) = scrCur
          r' = r - portalRow in
      case r' of
        z | z < 0 || z > height sq -> Nothing
          | c >= width sq -> error "splitBuffer returned invalid col"
          | otherwise -> Just (r', c + 1)

handleEvent :: TextWidget -> Square -> Event -> M ()
handleEvent tw sq e = do
  let move rd cd = liftIO $ do
        moveInternalCursor tw (columns sq) rd cd
        scrollToCursor tw sq
  case e of
    EventSpecialKey k -> do
      case k of
        KeyDownArrow -> move 1 0
        KeyUpArrow -> move (negate 1) 0
        KeyLeftArrow -> move 0 (negate 1)
        KeyRightArrow -> move 0 1
        _ -> return ()
    _ -> return ()

{-
splitBuffer :: Int {- ^ cursor index -} -> Int {- ^ columns -} -> String -> ([(Bool,String)], Maybe (Int, Int) {- ^ cursor -} )
splitBuffer ci cols str = (ret,mbScrCur) where
  -- state: (String left, Int linesMade, Int charsConsumed, Maybe (Int, Int) screen cursor)
  ((_, _, _, mbScrCur), ret) = execRWS (rb False) () (str, 0, 0, Nothing)
  peek i = take i . (\(s,_,_,_) -> s) <$> get
  consume i = do
    (s,!lm,!cc,sc) <- get
    let s' = drop i s
        cc' = cc + i
        sc' = case (sc, ci >= cc && ci < cc') of
                (Nothing, True) -> Just (lm, ci - cc)
                (Just _, True) -> error "cursor in two places!"
                (x, False) -> x
    put (s', lm + 1, cc', sc')
    return . not . null $ s'
  rb ov = do
    s <- peek (cols + 1)
    let lookahead = takeWhile (/= '\n') s
        ch = take cols lookahead
        overflow = length lookahead >= cols
    tell [(ov,ch)]
    continue <- consume (length ch + if overflow then 0 else 1)
    when continue (rb overflow)
    -}

moveInternalCursor tw cols rd cd = z where
  z = do
    buf <- readBuffer tw
    let wb = (buf, cols)
    fc <- readFileCoord tw
    let fc' = adjust wb fc rd cd
    writeIORef (fileCoord tw) fc'
  adjust wb fc x 0 | x < 0 = adjust wb (leftward wb fc) (x + 1) 0
                   | x == 0 = fc
                   | otherwise = adjust wb (rightward wb fc) (x - 1) 0
  adjust wb fc x y | y < 0 = adjust wb (upward wb fc) x (y + 1)
                   | otherwise = adjust wb (downward wb fc) x (y - 1)

countLines tw = length . lines <$> getText tw

readFileCoord = readIORef . fileCoord
readPortalTop = readIORef . portalTop
readBuffer = readIORef . buffer

{-
cursorIndex tw = do
  (r,c) <- readCursor tw
  t <- getText tw
  let prefix = take r (lines t)
  let ret = length (unlines prefix) + c
  putStrLn $ show (r,c) ++ ": " ++ show ret
  return ret
  -}

columns sq = width sq - 1

scrollToCursor tw sq = do
  let cols = columns sq
  buf <- readBuffer tw
  fileCoord <- readFileCoord tw
  t <- readPortalTop tw
  let wb = (buf, cols)
  let (r,c) = translate wb fileCoord
  let (pt,_) = translate wb t
  if r < pt
    then writeIORef (portalTop tw) (r,0) -- todo: replace zero with proper value
    else if r >= pt + height sq
           then writeIORef (portalTop tw) (r - height sq, 0) -- todo: fix zero AND fix r - height which assumes portal and file heights are the same
           else return ()
