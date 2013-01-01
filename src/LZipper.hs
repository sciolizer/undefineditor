{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module LZipper where

import Control.Applicative
-- import qualified Control.Lens as L
import Control.Lens hiding (Tape, rezip, saveTape, unsafelyRestoreTape, zipper)
import Data.Sequence.Lens
import Control.Monad
import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence ((><), ViewL((:<)))

data LZipper a = LZipper {
  _before :: S.Seq a,
  _after :: S.Seq a
  }

makeLenses ''LZipper

zipper :: [a] -> LZipper a
zipper xs = LZipper S.empty (S.fromList xs)

-- instance Indexable (Tape a) k
-- if the value is a Just, you can only set a Just
-- if the value is a Nothing, you can only set a Nothing
-- replacing a Nothing with a Just or a Just with a Nothing produces an error
focus :: SimpleIndexedLens (Tape a) (LZipper a) (Maybe a)
focus = indexed foo where -- $ \f lz@(LZipper (i,_) _) -> swapIn lz <$> f (Tape i) (current lz)
  foo :: (Functor f) => (Tape a -> (Maybe a) -> f (Maybe a)) -> LZipper a -> f (LZipper a)
  foo f lz@(LZipper bf _) = swapIn lz <$> f (Tape (S.length bf)) (current lz)
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
insert x = undefined -- over after (\(!i,xs) -> (i + 1, x : xs))

-- deleting at the end is a no-op
delete :: LZipper a -> LZipper a
delete = undefined

{-
deleteCheck :: LensLike ((,) (LZipper a)) s Bool (LZipper a) (LZipper a) -> LZipper a -> s -> (LZipper a, Bool)
deleteMaybe :: LZipper a -> Maybe (LZipper a)
deleteMaybe = undefined
-}

leftward, rightward :: MonadPlus m => LZipper a -> m (LZipper a)
leftward = undefined
rightward = undefined

leftmost, rightmost :: LZipper a -> LZipper a
leftmost = undefined
rightmost = undefined

tooth, teeth :: LZipper a -> Int
tooth = undefined
teeth = undefined

jerkTo :: MonadPlus m => Int -> LZipper a -> m (LZipper a)
jerkTo = undefined

tugTo :: Int -> LZipper a -> LZipper a
tugTo = undefined

rezip :: LZipper a -> [a]
rezip (LZipper l r) = toList (l >< r)

data Tape a = Tape Int

saveTape :: LZipper a -> Tape a
saveTape = undefined

restoreTape :: MonadPlus m => Tape a -> [a] -> m (LZipper a)
restoreTape = undefined

restoreNearTape :: MonadPlus m => Tape a -> [a] -> m (LZipper a)
restoreNearTape = undefined

unsafelyRestoreTape :: Tape a -> [a] -> LZipper a
unsafelyRestoreTape = undefined
