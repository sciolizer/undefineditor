{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module WebRemoting where

import Text.JSON

type Port = Int

data Page

data Listener

data Remote a

data Lifespan

reify :: Lifespan -> Remote a -> IO String
reify = undefined

-- probably need some kind of constraint on a...
eval :: Page -> Remote (String -> IO (Remote a))
eval = undefined

newListener :: Port -> (Page -> IO ()) -> IO ()
newListener = undefined

class Remotable a
-- instance JSON a => Remotable a
instance Remotable a => Remotable (IO a)

toRemote :: Remotable a => a -> Remote a
toRemote = undefined

{-

-}
