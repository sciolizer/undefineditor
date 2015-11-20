{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
module Restartable where

import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Functor.Identity
import Data.Typeable
import System.IO

import DependencyInjection

-- this should probably be a data type, not a class
data Restarter = Restarter
  { shutdown :: IO (B.ByteString, [Handle])
  , resume :: B.ByteString -> [Handle] -> IO () }

class Restartable a where
  restarter :: a -> Restarter

class Constructor a => RestartableConstructor a where
  -- accept :: a -> B.ByteString -> [Handle] -> IO ()

instance (Typeable a, Restartable a) => RestartableConstructor (IO a) where
{-
  accept m bs _ =
    case lookup (show $ typeRep m) bs of
      Nothing -> return ()
      Just b -> resume
      -}

instance (Typeable b, RestartableConstructor a) => RestartableConstructor (b -> a)

data RestartableBinding = RestartableBinding
  { rbName :: String
  , rbBinding :: Binding
  , rbResume :: Injector -> B.ByteString -> [Handle] -> IO () }

restartable :: (RestartableConstructor a) => String -> a -> RestartableBinding
restartable name x = RestartableBinding name (constructor x) rsm where
  rsm inj state files = do
    mbVal <- instantiate inj
    case mbVal of
      Left be -> ioError . userError $ "binding error on restart" -- todo: better error message
      Right v -> resume (restarter x) state files

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

