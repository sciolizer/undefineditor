{-# LANGUAGE
 GeneralizedNewtypeDeriving,
 NoMonomorphismRestriction
 #-}
module IDE.Undefineditor.Parser.Primitive (
  Parser(),
  newCommitment,
  next,
  runParser
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import qualified IDE.Undefineditor.Parser.Commit as C

newtype Parser a = Parser (StateT String C.Commit a)
  deriving (Alternative, Applicative, Functor, Monad, MonadIO)

newCommitment :: Parser (Parser ())
newCommitment = Parser $ do
  c <- lift (C.newCommitment)
  return (Parser (lift c))

next :: Parser (Maybe Char)
next = Parser $ do
  s <- get
  case s of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)

runParser :: Parser a -> String -> IO [(a, String)]
runParser (Parser s) str = C.runCommit (runStateT s str)
