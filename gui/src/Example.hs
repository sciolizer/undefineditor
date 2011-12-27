-- This file is not actually a part of undefineditor.
-- It's just a simple example of how to use the
-- Reactive framework.
module Main where

import Control.Concurrent.STM
import Control.Monad
import Graphics.UI.Gtk
import IDE.Undefineditor.Util.Reactive

main = do
  initGUI
  window <- windowNew
  vbox <- vBoxNew True 0
  containerAdd window vbox
  widgetShow vbox

  -- BEGIN from documentation
  rvars <- newRVars

  [(lentry, lrvar), (rentry, rrvar)] <- replicateM 2 $ do
    entry <- entryNew
    rvar <- newRVarIO rvars ""
    on entry editableChanged $ cleanly rvars . atomically . writeRVar rvar =<< entryGetText entry
    return (entry, rvar)

  label <- labelNew Nothing
  let concatenation = liftM2 (++) (readRVar lrvar) (readRVar rrvar)
  react (Just `fmap` concatenation) (\old new -> unless (old == new) $ set label [labelText := new])
  -- END from documentation

  boxPackStart vbox lentry PackNatural 0
  boxPackStart vbox rentry PackNatural 0
  boxPackStart vbox label PackNatural 0
  widgetShow lentry
  widgetShow rentry
  widgetShow label
  widgetShow window
  mainGUI
