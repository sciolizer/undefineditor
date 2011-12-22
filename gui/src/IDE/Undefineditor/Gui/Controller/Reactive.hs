{-# LANGUAGE
 GeneralizedNewtypeDeriving,
 NoMonomorphismRestriction,
 RankNTypes
 #-}

-- | Kind of like Functional Reactive Programming (FRP), but not really pure.
--
-- Typically in the MVC pattern, the model is encapsulated by the controller, and
-- model changes can be made only through the controller. The biggest drawback of this approach
-- is massive coupling - the controller typically becomes a big ball of mud, knowing way
-- more about the different components of the application than it needs to know. For instance,
-- if three otherwise unrelated parts of the view depend on the same part of a model, then the
-- controller code which handles the manipulation of that part of the model must make a reference
-- to all of the view components.
--
-- 'RVars' enable the controller logic to be spread across the entire application, so that
-- view update code can be written in sensible locations (such as when the view is constructed).
--
-- The model is stored in 'RVar's, which are kind of like 'TVar's (they support reading, writing, and waiting on changes), except that reactions to changes in the contents of a 'RVar' fire at a programmer-controlled time (when the 'cleanly' block exits).
--
-- To declare a dependency between the model and the view, use the 'react' function.
--
-- Here is an example, demonstrating how a label can be kept up to date with the concatenation
-- of the values in two text entry boxes.
--
-- >  rvars <- newRVars
-- >
-- >  -- Create two textboxes, with an rvar for each.
-- >  [(lentry, lrvar), (rentry, rrvar)] <- replicateM 2 $ do
-- >    entry <- entryNew
-- >    rvar <- newRVarIO rvars ""
-- >    -- modify the rvar whenever the text box changes
-- >    on entry editableChanged $ cleanly rvars . atomically . writeRVar rvar =<< entryGetText entry
-- >    return (entry, rvar)
-- >
-- >  -- Create a label...
-- >  label <- labelNew Nothing
-- >  -- and put the concatenation of the two textboxes into the label.
-- >  let concatenation = liftM2 (++) (readRVar lrvar) (readRVar rrvar)
-- >  react (Just `fmap` concatenation) (\old new -> unless (old == new) $ set label [labelText := new])
module IDE.Undefineditor.Gui.Controller.Reactive (

  -- * 'RVars' and 'RVar's
  RVars(),
  newRVars,

  RVar(),
  newRVar,
  newRVarIO,

  getRVars,

  readRVar,
  writeRVar,

  -- * The 'Stream' monad.
  Stream(),
  peekStream,

  -- * Controlling effects
  react,
  cleanly
) where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (STM(), TVar(), atomically, newTVar, newTVarIO, readTVar, readTVarIO, throwSTM, writeTVar)
import Control.Exception (finally)
import Control.Monad (join, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT(), runWriterT, tell)
import qualified Data.Map as M (Map(), elems, empty, insert, null)
import Data.Monoid (mappend)
import Data.Unique (Unique(), newUnique)

import IDE.Undefineditor.Util.ReentrantLock
import IDE.Undefineditor.Util.Safe

-- | A piece of the application's model. Each RVar belongs to one 'RVars'.
data RVar a = RVar
  RVars
  (TVar a) {- reactive variable -}
  (TVar (M.Map Unique Effect)) -- map of effects that must be triggered if this RVar is modified

-- | This monad can contain only pure computations and calls to 'readRVar'.
-- It is similar to the idea of a signal in FRP. The stream of values can be tapped into using the
-- 'react' function, which will fire a callback whenever the stream changes.
newtype Stream a = Stream (WriterT [UntypedRVar] STM a)
  deriving (Applicative, Functor, Monad)

-- | Accumulates the collection of updates to the view that have not yet been fired.
-- Every application must have at least one of these, and most will not have more.
data RVars = RVars (TVar (M.Map Unique Effect)) ReentrantLock

-- | Creates an instance of 'RVars'; usually only called at the beginning of your program.
newRVars :: IO RVars
newRVars = do
  tvar <- newTVarIO M.empty
  lock <- newReentrantLock
  return (RVars tvar lock)

-- | Gets the 'RVars' which tracks the side effects of this 'RVar'.
getRVars :: RVar a -> RVars
getRVars (RVar rvs _ _) = rvs

-- | Executes the reads in an 'Stream' transaction. This function returns
-- the current value in the stream produced by the 'Stream' computation.
--
-- Most of the time, calls to peekStream will be followed by a write to an 'RVar'. If your intention
-- is to update a view component based on the result of peekStream, you should probably be using
-- the 'react' function instead, which will tap into the ENTIRE stream of values produced
-- by the 'Stream' computation.
peekStream :: Stream a -> STM a
peekStream (Stream m) = fst `fmap` runWriterT m

-- | Creates a new 'RVar' with the given initial value and belonging to the given 'RVars'.
newRVar :: RVars -> a -> STM (RVar a)
newRVar = mkRVar newTVar

-- | Creates a new 'RVar' with the given initial value and belonging to the given 'RVars'. Uses
-- 'newTVarIO' underneath, for those times when you just can't use an 'atomically' block.
newRVarIO :: RVars -> a -> IO (RVar a)
newRVarIO = mkRVar newTVarIO

mkRVar :: (Monad m) => (forall a. a -> m (TVar a)) -> RVars -> a -> m (RVar a)
mkRVar mkr rvars x = do
  v <- mkr x
  dependencies <- mkr M.empty
  return (RVar rvars v dependencies)

-- | Reads the value of the given 'RVar'. Multiple reads can be composed in the 'Stream' monad, and
-- passed to the 'react' function.
readRVar :: RVar a -> Stream a
readRVar (RVar _ v deps) = Stream $ do
  tell [\u e -> modifyTVar deps (M.insert u e)] 
  lift $ readTVar v

modifyTVar tvar f = writeTVar tvar . f =<< readTVar tvar

-- | Modifies the value of an 'RVar'. The returned action MUST be executed in the context of a call
-- to 'cleanly'; an exception will be raised otherwise. View updaters which have been registered
-- with the 'react' function will fire when the 'cleanly' block exits.
writeRVar :: RVar a -> a -> STM ()
writeRVar (RVar (RVars effects lock) v deps) x = do
  ls <- lockState lock
  when (ls /= Locked) $
    throwSTM (userError "attempted to writeRVar outside the context of a cleanly")
    -- exitWith (ExitFailure 1)
  -- todo: periodically clean out the deps which aren't doing anything (this will require some kind of data change, as the only signal that the action is inert is hidden in a closure generated by the mkChangingAction function)
  -- accumulate side effects in the RVars
  d <- readTVar deps
  effs <- readTVar effects
  writeTVar effects (d `mappend` effs)
  -- and actually write the new value
  writeTVar v x

-- | Fires some of the callback functions registered with 'react', based on
-- whichever 'RVar's have been modified with calls to 'writeRVar'.
--
-- The first implementation of this used a custom monad to ensure that
-- 'writeRVar' computations could not be run outside the context of a 'cleanly'
-- block, but that was at odds with gtk signal callbacks, which must
-- be in the IO monad. So just be careful.
--
-- Calls to 'cleanly' are re-entrant, so you can safely use this
-- function in all of your gtk signal handlers, even when they fire
-- programmatically. Callback functions accumulated from nested calls
-- to 'cleanly' are fired in LIFO order - that is, the callbacks of an
-- inner call to cleanly will finish before the callbacks of an outer call.
-- The outermost invocation of cleanly will not return until 'RVar' modifications
-- cease to propogate. It IS possible for 'cleanly' to get caught in an infinite loop.
cleanly
  :: RVars -- ^ Accumulator of side effects.
  -> IO () -- ^ action which contains calls to 'writeRVar'.
  -> IO () -- ^ action with callback invocations appended
cleanly (RVars effs lock) action = ret where
  ret = do
    withLock lock (\b -> action `finally` run b)
  run b = do
    join $ atomically $ do
      effects <- readTVar effs
      act <- mapM_ safe `fmap` sequence (M.elems effects)
      writeTVar effs M.empty
      return act
    when b $ do
      effects <- readTVarIO effs
      unless (M.null effects) (run b) -- more TVars were written in the propogation of effects, so rerun

-- | Registers a callback function with a stream of values. This function
-- returns immediatley.
react
  :: Stream (Maybe a) -- ^ The stream of values to monitor. Changes in the output of the 'Stream' computation will trigger invocations of the callback function. If the 'Stream' computation returns 'Nothing', then the callback is unregistered, does not fire, and will never fire again. i.e. 'Nothing' signals the EOF of the stream.
  -> (a -> a -> IO ()) -- ^ The callback which updates the view. The second argument (new) is the result of the 'Stream' computation. The first argument (old) is whatever value was previously the second argument. If the 'Stream' computation fires multiple times in a single 'cleanly' block, only the most recent result will be passed to the callback. The callback will never see the intermittent values, either as the old argument or the new argument. Since changes to the view have a tendency to fire OTHER registered callbacks (esp. if you are using gtk signals), there is a risk of infinite propogation. For this reason, it is recommend that all callbacks check the old and new arguments for equality, and do nothing if they are equal. The callback should never raise an exception.
  -> IO (Maybe a) -- ^ The first value of the 'Stream' stream, which will be used as the old argument the first time the callback is invoked. This value can usually be ignored, since 'react' is usually called at the same time as view construction, when everything is in a known state.
react (Stream reactee) action = do
  unique <- newUnique
  atomically $ do
    let valMaker = runWriterT reactee
    (a, vars) <- valMaker
    case a of
      Nothing -> return Nothing
      Just z -> do
        e <- mkChangingAction z valMaker action (\e rv -> rv unique e)
        mapM_ (\inst -> inst unique e) vars
        return (Just z)

mkChangingAction :: a -> STM (Maybe a, [UntypedRVar]) -> (a -> a -> IO ()) -> (Effect -> UntypedRVar -> STM ()) -> STM Effect
mkChangingAction oldValue newValueMaker action_ inst = do
  oldValueTVar <- newTVar oldValue
  tvarAct <- newTVar (Just action_)
  let ret = do
        action <- readTVar tvarAct
        case action of
          Nothing -> return (return ()) -- this action has been disabled
          Just act -> do
            ov <- readTVar oldValueTVar
            (nv, vars) <- newValueMaker
            case nv of
              Nothing -> do
                -- disable this action
                writeTVar tvarAct Nothing -- allows action (which probably holds references to widget) to be garbage collected
                return (return ())
              Just z -> do
                writeTVar oldValueTVar z
                mapM_ ((inst :: (Effect -> UntypedRVar -> STM ())) (ret :: Effect)) (vars :: [UntypedRVar])
                return (act ov z)
  return ret

type Effect = STM (IO ()) -- it returns a DIFFERENT IO action each time... each time you invoke this, you discard a new 'old value'
type UntypedRVar = Unique -> Effect -> STM () -- just the function which installs effects
