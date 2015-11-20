{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
module Restart where

import Control.Applicative
import Control.Exception
import qualified Data.ByteString as B
import System.Environment
import System.IO
import System.IO.Error
import System.Process

-- handles to be implemented later
-- eventually this will be more clever and try to detect
-- if the subsequent process restarted successfully or not
-- for now it just assumes it will
restart :: B.ByteString -> [Handle] -> IO Bool
restart state _ = do
  B.writeFile "restart.state" state
  ep <- getExecutablePath
  args <- getArgs
  _ <- spawnProcess ep args
  return True

resume :: IO (Maybe (B.ByteString, [Handle]))
resume = do
  catchJust
    (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just Nothing else Nothing)
    (Just . (,[]) <$> B.readFile "restart.state")
    return
