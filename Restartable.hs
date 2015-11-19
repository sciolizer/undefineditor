{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
module Restartable where

import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Functor.Identity
import Data.Typeable

import DependencyInjection

class Restartable a where
  shutdown :: (Typeable b, Binary b) => a -> IO b
  resume :: (Typeable b, Binary b) => a -> b -> IO ()

class Constructor a => RestartableConstructor a
instance (Typeable a, Restartable a) => RestartableConstructor (Identity a)
instance (Typeable a, Restartable a) => RestartableConstructor (IO a)
instance (Typeable b, RestartableConstructor a) => RestartableConstructor (b -> a)

data RestartableBinding

restartable :: (RestartableConstructor a) => a -> RestartableBinding
restartable = undefined

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

