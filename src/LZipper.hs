{-# LANGUAGE FlexibleContexts #-}
module LZipper where

import qualified Control.Lens as L
import Control.Monad

data LZipper a

zipper :: a -> LZipper a
zipper = undefined

focus :: L.SimpleIndexedLens (Tape (LZipper a)) (LZipper a) a
focus = undefined

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
rezip = undefined

data Tape a

saveTape :: LZipper a -> Tape a
saveTape = undefined

restoreTape :: MonadPlus m => Tape a -> a -> m (LZipper a)
restoreTape = undefined

restoreNearTape :: MonadPlus m => Tape a -> a -> m (LZipper a)
restoreNearTape = undefined

unsafelyRestoreTape :: Tape a -> a -> LZipper a
unsafelyRestoreTape = undefined
