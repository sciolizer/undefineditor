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

data TextWidget = TextWidget {
  contents :: IORef String,
  index :: IORef Int, -- start of viewable portion
  cursor :: IORef (Int, Int)
  }

newTextWidget :: IO TextWidget
newTextWidget = TextWidget <$> newIORef "" <*> newIORef 0 <*> newIORef (0,0)

getText :: TextWidget -> IO String
getText = readIORef . contents

setText :: TextWidget -> String -> IO ()
setText tw newc = writeIORef (contents tw) newc

render :: TextWidget -> Square -> M (Maybe (Int, Int)) {- ^ row, col for cursor, relative to square -}
render tw sq = do
  w <- defaultWindow
  cs <- liftIO $ getText tw
  ind <- liftIO $ readIndex tw
  crsr <- liftIO $ cursorIndex tw -- todo: change cursor to int
  let rest = drop ind cs
  updateWindow w $ do
    let (ls, scrCur) = splitBuffer (crsr - ind) (width sq - 1) rest
    forM_ (zip [0..(height sq - 1)] ls) $ \(line, (overflow, content)) -> do
      when overflow $ do
        moveCursorSquare sq line 0
        drawString "\\"
      moveCursorSquare sq line 1
      drawString content
    return $ case scrCur of
      Nothing -> Nothing
      Just (r,c) | r >= height sq -> Nothing
                 | c >= width sq -> error "splitBuffer returned invalid col"
                 | otherwise -> Just (r, c + 1)

handleEvent :: TextWidget -> Square -> Event -> M ()
handleEvent tw sq e =
  case e of
    EventSpecialKey k ->
      case k of
        KeyDownArrow -> liftIO $ do
          moveInternalCursor tw 1 0
          scrollToCursor tw sq
        _ -> return ()
    _ -> return ()
    

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

moveInternalCursor tw rd cd = do
  ls <- countLines tw
  let d (r,c) = (bound (r + rd), max 0 (c + cd))
      bound x = if x < 0 then 0 else if x > ls then ls else x
  modifyIORef (cursor tw) d

countLines tw = length . lines <$> getText tw

readCursor = readIORef . cursor
readIndex = readIORef . index

cursorIndex tw = do
  (r,c) <- readCursor tw
  t <- getText tw
  let prefix = take r (lines t)
  return (length (unlines prefix) + c)

scrollToCursor tw sq = do
  cs <- getText tw
  (r,c) <- readCursor tw
  ind <- readIndex tw
  let prefix = take ind cs
  if r < length (lines prefix)
    then writeIORef (index tw) . length . unlines . take r . lines $ cs
    else return () -- todo
