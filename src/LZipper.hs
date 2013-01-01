{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module LZipper where

import Control.Applicative
import Control.Lens hiding (Tape, leftward, rezip, rightward, saveTape, tooth, unsafelyRestoreTape, zipper)
import Data.Sequence.Lens
import Control.Monad
import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence ((><), (<|), ViewL((:<)))

data LZipper a = LZipper {
  _before :: S.Seq a, -- reversed
  _after :: S.Seq a
  }

makeLenses ''LZipper

zipper :: [a] -> LZipper a
zipper xs = LZipper S.empty (S.fromList xs)

focus :: SimpleIndexedLens Int (LZipper a) (Maybe a)
focus = indexed foo where
  foo :: (Functor f) => (Int -> (Maybe a) -> f (Maybe a)) -> LZipper a -> f (LZipper a)
  foo f lz@(LZipper bf _) = swapIn lz <$> f (S.length bf) (current lz)
  swapIn :: LZipper a -> Maybe a -> LZipper a
  swapIn lz val = over (after.viewL) changeHead lz where
    changeHead S.EmptyL =
      case val of
        Nothing -> S.EmptyL
        Just _ -> invalidArgument "cannot set at end of list; use insert instead"
    changeHead (_ :< xs) =
      case val of
        Nothing -> invalidArgument "cannot set Nothing in the middle of the list; use delete instead"
        Just x' -> x' :< xs
  current :: LZipper a -> Maybe a
  current = view (after . viewL . to safeHead) where
    safeHead S.EmptyL = Nothing
    safeHead (x :< _) = Just x
  invalidArgument = error

-- usage: over myZipper (insert 'x')
insert :: a -> LZipper a -> LZipper a
insert x = over after (x <|)

-- deleting at the end is a no-op
delete :: LZipper a -> LZipper a
delete = over after (S.drop 1)

{-
deleteCheck :: LensLike ((,) (LZipper a)) s Bool (LZipper a) (LZipper a) -> LZipper a -> s -> (LZipper a, Bool)
deleteMaybe :: LZipper a -> Maybe (LZipper a)
deleteMaybe = undefined
-}

leftward, rightward :: MonadPlus m => LZipper a -> m (LZipper a)
leftward (LZipper bf af) = uncurry (flip LZipper) `liftM` (af `shiftOnto` bf)
rightward (LZipper bf af) = uncurry LZipper `liftM` (bf `shiftOnto` af)

x `shiftOnto` y =
  case S.viewl x of
    S.EmptyL -> mzero
    z :< x' -> return (x', z <| y)

leftmost, rightmost :: LZipper a -> LZipper a
leftmost = farthest leftward
rightmost = farthest rightward

tooth, teeth :: LZipper a -> Int
tooth (LZipper bf _) = S.length bf
teeth (LZipper bf af) = S.length bf + S.length af

jerkTo :: MonadPlus m => Int -> LZipper a -> m (LZipper a)
jerkTo n z = ret where
  k = tooth z
  ret = case compare k n of
          LT -> jerks rightward (n - k) z
          EQ -> return z
          GT -> jerks leftward (k - n) z

tugTo :: Int -> LZipper a -> LZipper a
tugTo n z = ret where
  k = tooth z
  ret = case compare k n of
          LT -> tugs rightward (n - k) z
          EQ -> z
          GT -> tugs leftward (k - n) z

rezip :: LZipper a -> [a]
rezip (LZipper l r) = toList (S.reverse l >< r)

{-
data Tape a = Tape Int

saveTape :: LZipper a -> Tape a
saveTape = Tape . teeth

restoreTape :: MonadPlus m => Tape a -> [a] -> m (LZipper a)
restoreTape = undefined

restoreNearTape :: MonadPlus m => Tape a -> [a] -> m (LZipper a)
restoreNearTape = undefined

unsafelyRestoreTape :: Tape a -> [a] -> LZipper a
unsafelyRestoreTape = undefined
-}
