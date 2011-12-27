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
module IDE.Undefineditor.Util.Reactive (

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
  cleanly,

  -- * Convenience functions
  cleanlyWriteRVar,
  reactIO
) where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (STM(), TVar(), atomically, newTVar, newTVarIO, readTVar, readTVarIO, throwSTM, writeTVar)
import Control.Exception (finally)
import Control.Monad (join, liftM, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT(), runWriterT, tell)
import Data.Function (on)
import Data.Monoid (mappend)
import qualified Data.Set as S (Set(), delete, elems, empty, insert, null, toList)
import Data.Unique (Unique(), newUnique)

import IDE.Undefineditor.Util.ReentrantLock
import IDE.Undefineditor.Util.Safe

-- | A piece of the application's model. Each RVar belongs to one 'RVars'.
data RVar a = RVar {
  -- | Gets the 'RVars' which tracks the side effects of this 'RVar'.
  getRVars :: RVars,
  rvarCounter :: Int,
  rvarContents :: TVar a,
  rvarWatchers :: (TVar (S.Set Effect)) -- effects that must be triggered if this RVar is modified
  }

-- instance Eq RVar where (==) = (==) `on` rvarIdentity
-- instance Ord RVar where compare = compare `on` rvarIdentity

rvarIdentity :: RVar a -> (RVars, Int)
rvarIdentity rvar = (getRVars rvar, rvarCounter rvar)

-- | This monad can contain only pure computations and calls to 'readRVar'.
-- It is similar to the idea of a signal in FRP. The stream of values can be tapped into using the
-- 'react' function, which will fire a callback whenever the stream changes.
newtype Stream a = Stream (WriterT [UntypedRVar] STM a)
  deriving (Applicative, Functor, Monad)

-- | Accumulates the collection of updates to the view that have not yet been fired.
-- Every application must have at least one of these, and most will not have more.
data RVars = RVars {
  rvarsEffects :: TVar (S.Set Effect),
  rvarsLock :: ReentrantLock,
  rvarsIdentity :: Unique,
  rvarsCounter :: TVar Int
  }

instance Eq RVars where (==) = (==) `on` rvarsIdentity
instance Ord RVars where compare = compare `on` rvarsIdentity

-- | Creates an instance of 'RVars'; usually only called at the beginning of your program.
newRVars :: IO RVars
newRVars = do
  tvar <- newTVarIO S.empty
  lock <- newReentrantLock
  unique <- newUnique
  counter <- newTVarIO 0
  return (RVars tvar lock unique counter)

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
newRVar = mkRVar newTVar id

-- | Creates a new 'RVar' with the given initial value and belonging to the given 'RVars'. Uses
-- 'newTVarIO' underneath, for those times when you just can't use an 'atomically' block.
newRVarIO :: RVars -> a -> IO (RVar a)
newRVarIO = mkRVar newTVarIO atomically

mkRVar :: (Monad m) => (forall b. b -> m (TVar b)) -> (forall c. STM c -> m c) -> RVars -> a -> m (RVar a)
mkRVar mkr lift rvars x = do
  v <- mkr x
  dependencies <- mkr S.empty
  rid <- lift (nextId rvars)
  return (RVar rvars rid v dependencies)

nextId :: RVars -> STM Int
nextId = liftM fst . (`modifyTVar` (+1)) . rvarsCounter

-- | Reads the value of the given 'RVar'. Multiple reads can be composed in the 'Stream' monad, and
-- passed to the 'react' function.
readRVar :: RVar a -> Stream a
readRVar rvar = Stream $ do
  let modify = void . modifyTVar (rvarWatchers rvar)
  tell [UntypedRVar (modify . S.insert) (modify . S.delete) (rvarIdentity rvar)]
  lift $ readTVar (rvarContents rvar)

modifyTVar :: TVar a -> (a -> a) -> STM (a, a)
modifyTVar tvar f = do
  old <- readTVar tvar
  let new = f old
  writeTVar tvar new
  return (old, new)

swapTVar tvar val = liftM fst $ modifyTVar tvar (const val)

-- | Modifies the value of an 'RVar'. The returned action MUST be executed in the context of a call
-- to 'cleanly'; an exception will be raised otherwise. View updaters which have been registered
-- with the 'react' function will fire when the 'cleanly' block exits.
writeRVar :: RVar a -> a -> STM ()
writeRVar (RVar rvars _ v deps) x = do
  ls <- lockState (rvarsLock rvars)
  when (ls /= Locked) $
    throwSTM (userError "attempted to writeRVar outside the context of a cleanly")
    -- exitWith (ExitFailure 1)
  -- todo: periodically clean out the deps which aren't doing anything (this will require some kind of data change, as the only signal that the action is inert is hidden in a closure generated by the mkChangingAction function)
  -- accumulate side effects in the RVars
  d <- readTVar deps
  modifyTVar (rvarsEffects rvars) (d `mappend`)
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
cleanly rvars action = ret where
  ret = do
    withLock (rvarsLock rvars) (\b -> action `finally` run b)
  run b = do
    let effs = rvarsEffects rvars
    join $ atomically $ do
      effects <- swapTVar effs S.empty
      act <- mapM_ safe `fmap` mapM makeCallback (S.elems effects)
      return act
    when b $ do
      effects <- readTVarIO effs
      unless (S.null effects) (run b) -- more TVars were written in the propogation of effects, so rerun

-- | Registers a callback function with a stream of values. This function
-- returns immediatley.
react
  :: Stream (Maybe a) -- ^ The stream of values to monitor. Changes in the output of the 'Stream' computation will trigger invocations of the callback function. If the 'Stream' computation returns 'Nothing', then the callback is unregistered, does not fire, and will never fire again. i.e. 'Nothing' signals the EOF of the stream.
  -> (a -> a -> IO ()) -- ^ The callback which updates the view. The second argument (new) is the result of the 'Stream' computation. The first argument (old) is whatever value was previously the second argument. If the 'Stream' computation fires multiple times in a single 'cleanly' block, only the most recent result will be passed to the callback. The callback will never see the intermittent values, either as the old argument or the new argument. Since changes to the view have a tendency to fire OTHER registered callbacks (esp. if you are using gtk signals), there is a risk of infinite propogation. For this reason, it is recommend that all callbacks check the old and new arguments for equality, and do nothing if they are equal. The callback should never raise an exception.
  -> STM (Maybe a) -- ^ The first value of the 'Stream' stream, which will be used as the old argument the first time the callback is invoked. This value can usually be ignored, since 'react' is usually called at the same time as view construction, when everything is in a known state.
react (Stream reactee) action = do
  let valMaker = runWriterT reactee
  (a, vs) <- valMaker
  case (a, vs) of
    (_, []) -> return a -- no reading from vars actually occurred, so this Stream is pure; no registration of effects is necessary, since they will never fire
    (Nothing, _) -> return Nothing -- stream immediately unregistered itself
    (Just z, vars@(v:_)) -> do
      let (rvars, _) = untypedRVarIdentity v -- it's possible for the vars to share multiple RVars, but that doesn't invalidate the safety of picking just one to represent the identity of this registered callback
      iden <- nextId rvars
      e <- mkChangingAction z valMaker action (rvars, iden)
      mapM_ (bindEffect e) vars
      return (Just z)

-- | Same as 'react', but runs in the IO monad. It is NOT safe to call this function from the context of
-- an unsafePerformIO.
reactIO :: Stream (Maybe a) -> (a -> a -> IO ()) -> IO (Maybe a)
reactIO stream callback = atomically $ react stream callback

type ReactionIdentity = (RVars, Int)

-- todo: garbage collection of these things should really not be that hard...
--   change UntypedRVar to include BOTH the installer AND the uninstaller...
--   change Effect to include both the "create callback" function AND the
--   "uninstall all installations" function.... everytime the "create callback"
--   function is called, it should change the behavior of the "uninstall all
--   installations" function (and thanks to STM, we don't have to worry about
--   the two stepping on each other)
mkChangingAction :: a -> STM (Maybe a, [UntypedRVar]) -> (a -> a -> IO ()) -> ReactionIdentity -> STM Effect
mkChangingAction oldValue newValueMaker action_ unique = do
  oldValueTVar <- newTVar oldValue
  installees <- newTVar S.empty
  let ret = Effect unique mc installees
      mc = do
        ov <- readTVar oldValueTVar
        (nv, vars) <- newValueMaker
        case nv of
          Nothing -> do
            disableEffect ret
            return (return ())
          Just z -> do
            writeTVar oldValueTVar z
            mapM_ (bindEffect ret) vars
            return (action_ ov z)
  return ret

data Effect = Effect {
  effectIdentifier :: ReactionIdentity,
  makeCallback :: STM (IO ()), -- it returns a DIFFERENT IO action each time... each time you invoke this, you discard a new 'old value'
  installedRVars :: TVar (S.Set UntypedRVar)
  }

instance Eq Effect where (==) = (==) `on` effectIdentifier
instance Ord Effect where compare = compare `on` effectIdentifier

disableEffect :: Effect -> STM ()
disableEffect effect = mapM_ (`uninstallEffect` effect) . S.toList =<< readTVar (installedRVars effect)

data UntypedRVar = UntypedRVar {
  installEffect :: Effect -> STM (),
  uninstallEffect :: Effect -> STM (),
  untypedRVarIdentity :: (RVars, Int)
  }

instance Eq UntypedRVar where (==) = (==) `on` untypedRVarIdentity
instance Ord UntypedRVar where compare = compare `on` untypedRVarIdentity

bindEffect :: Effect -> UntypedRVar -> STM ()
bindEffect effect rvar = do
  modifyTVar (installedRVars effect) (S.insert rvar)
  installEffect rvar effect

-- | Assigns the given value to the given rvar and fires all related side effects.
cleanlyWriteRVar :: RVar a -> a -> IO ()
cleanlyWriteRVar r@(RVar rs _ _ _) v = cleanly rs . atomically . writeRVar r $ v
