module IDE.Undefineditor.Parser.Commit (
  Commit(),
  runCommit,
  newCommitment
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S
import Data.Unique

data Commit a =
    None String
  | One a
  | Choice (Commit a) (Commit a)
  | NewCommitment Unique (Commit a)
  | Committed Unique (Commit a)
  | Action (IO (Commit a))

instance Applicative Commit where
  pure = return
  (<*>) = ap
instance Functor Commit where fmap = liftM
instance Monad Commit where
  return = One
  None s >>= _ = None s
  One a >>= f = f a
  NewCommitment u ct >>= f = NewCommitment u (f =<< ct)
  Committed u ct >>= f = Committed u (f =<< ct)
  Choice ctl ctr >>= f = Choice (f =<< ctl) (f =<< ctr)
  Action act >>= f = Action $ do
    ct <- act
    return (f =<< ct)
  fail = None

instance MonadIO Commit where
  liftIO act = Action (liftM One act)

instance MonadPlus Commit where
  mzero = None "mzero"
  mplus = Choice -- todo: I think this is wrong, because (<|>) is left associative

newCommitment :: Commit (Commit ())
newCommitment = Action $ do
  u <- newUnique
  return $ NewCommitment u $ One $ Committed u $ One ()

runCommit :: Commit a -> IO [a]
runCommit = liftM fst . flatten' S.empty

flatten' :: S.Set Unique -> Commit a -> IO ([a], S.Set Unique)
flatten' enc com =
  case com of
    None _ -> return ([], S.empty)
    One x -> return ([x], S.empty)
    NewCommitment u c -> flatten' (S.insert u enc) c
    Committed u c -> do
      when (u `S.notMember` enc) $ putStrLn "Warning: commitment tree inconsistent"
      liftM (second (S.insert u)) (flatten' enc c)
    Action act -> do
      c <- act
      flatten' enc c
    Choice c1 c2 -> do
      ll@(left, usl) <- flatten' enc c1
      if usl `S.isSubsetOf` enc then
        return ll else do
        (right, usr) <- flatten' enc c2
        return (left ++ right, usl `S.union` usr)
