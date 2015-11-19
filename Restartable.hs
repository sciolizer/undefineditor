{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
module Restartable where

import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Functor.Identity
import Data.Typeable

import DependencyInjection

-- this should probably be a data type, not a class
class Restartable a where
  shutdown :: a -> IO B.ByteString
  resume :: a -> B.ByteString -> IO ()

class Constructor a => RestartableConstructor a where
  accept :: a -> [(TypeRepString,B.ByteString)] -> IO ()

instance (Typeable a, Restartable a) => RestartableConstructor (Identity a) where
  accept (Identity x) bs =
    case lookup (show $ typeRep (Identity x)) bs of
      Nothing -> return ()
      Just b -> resume x b

instance (Typeable a, Restartable a) => RestartableConstructor (IO a) where
  accept m bs =
    case lookup (show $ typeRep m) bs of
      Nothing -> return ()
      Just b -> resume

instance (Typeable b, RestartableConstructor a) => RestartableConstructor (b -> a)

data RestartableBinding = RestartableBinding {
  rbBinding :: Binding,
  rbResume :: [(TypeRepString,B.ByteString)] -> IO ()
  }

restartable :: (RestartableConstructor a) => a -> RestartableBinding
restartable x = RestartableBinding (constructor x) (accept x) where

type TypeRepString = String

-- byte string collection can be empty if there was no previous state
-- all resumption is on a best effort basis. Some bytestrings may be
-- ignored if their TypeRep does not match the latest.
resumeAll :: (Typeable a) => [RestartableBinding] -> [Binding] -> [(TypeRepString,B.ByteString)] -> IO a
resumeAll = undefined

data Top = Top
  deriving (Eq, Ord, Read, Show, Typeable)

data a :- b = a :- b
  deriving (Eq, Ord, Read, Show, Typeable)

chainRestartableBinding :: a -> b :- a
chainRestartableBinding = undefined

{-
could just do
Nil
Cons Dynamic Nil
Cons Dynamic (Cons Dynamic Nil)
etc
-}

data Root a

data Succ a

-- withRB :: RestartableBinding -> a -> Succ a

