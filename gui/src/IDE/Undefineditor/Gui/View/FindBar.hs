-- | Functions for working with the quick-find bar.
--
-- There is one find bar per tab.
module IDE.Undefineditor.Gui.View.FindBar (
  -- * Instantiation
  FindBar(),
  newFindBar,

  -- * Getters
  findBarQuery,
  findBarWidget,

  -- * Actions
  focusFindBar,
  hideFindBar,
  findNext,
  findPrevious
) where

import Control.Concurrent.STM (atomically)
import Control.Monad (liftM, unless, void)
import Data.Maybe (fromMaybe)
import Graphics.UI.Gtk (
  AttrOp((:=)),
  Entry(),
  Packing(PackGrow),
  TextBuffer(),
  TextBufferClass(),
  TextIter(),
  TextMark(),
  TextTag(),
  TextView(),
  Widget(),
  boxPackStart,
  editableChanged,
  entryGetText,
  entryNew,
  hBoxNew,
  on,
  set,
  textBufferApplyTag,
  textBufferGetEndIter,
  textBufferGetInsert,
  textBufferGetIterAtMark,
  textBufferGetSelectionBound,
  textBufferGetStartIter,
  textBufferGetTagTable,
  textBufferMoveMark,
  textBufferRemoveTag,
  textIterBackwardSearch,
  textIterCompare,
  textIterForwardSearch,
  textTagBackground,
  textTagNew,
  textTagTableAdd,
  textViewGetBuffer,
  textViewScrollToIter,
  toWidget,
  widgetGrabFocus,
  widgetHide,
  widgetShow
  )

import IDE.Undefineditor.Gui.Controller.Reactive

-- | A quick-find bar bound to some text view.
data FindBar = FindBar {
  queryVar :: RVar String,
  -- | The widget representing the entire bar. Usually you will want to pack the bar into a 'VBox'.
  findBarWidget :: Widget,
  entryBox :: Entry,
  findBarTextView :: TextView,
  highlightTag :: TextTag
  }

-- | Creates a new FindBar, and binds its actions to the given text view.
--
-- You can access the created widget using 'findBarWidget'.
newFindBar :: RVars -> TextView -> IO FindBar
newFindBar rvars tv = do
  findBox <- hBoxNew False 0 -- the hbox is kind of unnecessary for now, but that will change
  entry <- entryNew
  widgetShow entry
  rvar <- newRVarIO rvars ""
  boxPackStart findBox entry PackGrow 0
  on entry editableChanged $ cleanlyWriteRVar rvar =<< entryGetText entry
  tb <- textViewGetBuffer tv
  tag <- newHighlightTag tb
  let ret = FindBar rvar (toWidget findBox) entry tv tag
  reactIO (Just `fmap` findBarQuery ret) $ \old new -> unless (old == new) $ highlightAll ret new
  return ret

highlightAll fb new = do
  let tv = findBarTextView fb
  tb <- textViewGetBuffer tv
  let tag = highlightTag fb
  start <- textBufferGetStartIter tb
  end <- textBufferGetEndIter tb
  insert <- textBufferGetIterAtMark tb =<< textBufferGetInsert tb
  textBufferRemoveTag tb tag start end -- removes all highlighting tags
  unless (null new) $ do
    iters <- (`unfoldM` start) $ \st -> do
      mbIters <- searchForwardFrom st new
      case mbIters of
        Nothing -> return Nothing
        Just (left, right) -> do
          textBufferApplyTag tb tag left right
          return (Just (left, right))
    unless (null iters) $ do
      next <- findM (\i -> (/= LT) `fmap` textIterCompare insert i) iters
      scrollTo tv (fromMaybe (last iters) next)

scrollTo tv i = void $ textViewScrollToIter tv i 0.0 (Just (0.5, 0.5))

unfoldM :: (Monad m) => (b -> m (Maybe (a,b))) -> b -> m [a]
unfoldM gen seed = do
  mbNext <- gen seed
  case mbNext of
    Nothing -> return []
    Just (v, nseed) -> (v:) `liftM` unfoldM gen nseed

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM pred (x:xs) = do
  b <- pred x
  if b then return (Just x) else findM pred xs

-- | Shows the find bar if it is not already shown, and transfers focus to the entry box.
--
-- If a query already exists in the find bar, then highlights the occurences in the text buffer.
focusFindBar :: FindBar -> IO ()
focusFindBar fb = do
  let entry = entryBox fb
  widgetShow (findBarWidget fb)
  widgetGrabFocus entry
  highlightAll fb =<< entryGetText entry

-- | Hides the find bar from view.
--
-- Any highlighted text is hidden, the view is scrolled back to the cursor, and focus is returned
-- to the text view.
hideFindBar :: FindBar -> IO ()
hideFindBar fb = do
  widgetHide (findBarWidget fb)
  let tv = findBarTextView fb
  tb <- textViewGetBuffer tv
  start <- textBufferGetStartIter tb
  end <- textBufferGetEndIter tb
  textBufferRemoveTag tb (highlightTag fb) start end -- removes all highlighting tags
  insert <- textBufferGetIterAtMark tb =<< textBufferGetInsert tb
  textViewScrollToIter tv insert 0.0 (Just (0.5, 0.5))
  widgetGrabFocus tv

findDirection directionFind which fb = do
  let tv = findBarTextView fb
  tb <- textViewGetBuffer tv
  (selectionBoundMark, selectionBound) <- markAndIter tb textBufferGetSelectionBound
  (insertMark, insert) <- markAndIter tb textBufferGetInsert
  query <- atomically $ peekStream (findBarQuery fb)
  mbIters <- directionFind (which (insert, selectionBound)) query -- todo: wrap search around
  case mbIters of
    Nothing -> return ()
    Just (l,r) -> do
    textBufferMoveMark tb insertMark l
    textBufferMoveMark tb selectionBoundMark r
    scrollTo tv l
    widgetGrabFocus tv

markAndIter :: (TextBufferClass tb) => tb -> (tb -> IO TextMark) -> IO (TextMark, TextIter)
markAndIter tb getMark = do
  mark <- getMark tb
  iter <- textBufferGetIterAtMark tb mark
  return (mark, iter)

searchForwardFrom start query = textIterForwardSearch start query [] Nothing
searchBackwardFrom start query = textIterBackwardSearch start query [] Nothing

-- | Moves the insert mark forward until a match is found. The match is selected.
findNext :: FindBar -> IO ()
findNext = findDirection searchForwardFrom snd

-- | Moves the insert mark backward until a match is found. The match is selected.
findPrevious :: FindBar -> IO ()
findPrevious = findDirection searchBackwardFrom fst

-- | The value inside the entry box in the quick-find bar.
findBarQuery :: FindBar -> Stream String
findBarQuery = readRVar . queryVar

newHighlightTag :: TextBuffer -> IO TextTag
newHighlightTag tb = do
  table <- textBufferGetTagTable tb
  tag <- textTagNew Nothing
  set tag [textTagBackground := "yellow"]
  textTagTableAdd table tag
  return tag
