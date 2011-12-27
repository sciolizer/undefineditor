{-# LANGUAGE
 FlexibleInstances
 #-}
module IDE.Undefineditor.Util.Disposable (
  Disposable
) where

import Data.Monoid

type Disposable a = IO (a, IO ())

instance Monoid (IO ()) where
  mempty = return ()
  mappend x y = x >> y
