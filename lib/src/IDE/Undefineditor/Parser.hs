-- Why create another parser?
--   Mainly because none of the existing parsers
--   I've looked at have the equivalent of the
--   'newCommitment' function.
--
--   Polyparse has something like it, but as far
--   as I can tell, the commit function has static
--   extenet, and I would like dynamic extent.
--
--   Even if I am mistaken, polyparse does not
--   support (as far as I can tell) wrapping the IO
--   monad.
--
--   Note that technically, the 'Parser' type in
--   this module is not a true monad because it wraps
--   the IO monad and supports backtracking.
--   So sue me. It's damn convenient.
--
-- The alternative instance induces a branch
-- in the search for a valid parse. If your
-- parser struggles with commitment, then EVERY
-- branch will be attempted. e.g.

-- >>> one <|> two <|> three
-- >>> fail "forced fail"
--
-- This example code will attempt all three
-- parsers before failing. If you would like
-- to commit to the first alternative that succeeds
-- on its piece of the data, then either use
-- the convience function 'disjoint', or else use
-- 'newCommitment' for maximum flexibility.
module IDE.Undefineditor.Parser (
  Parser(),
  char,
  disjoint,
  exactly,
  -- many,
  newCommitment,
  next,
  oneOf,
  runParser
) where

import Control.Applicative

import IDE.Undefineditor.Parser.Primitive

disjoint :: [Parser a] -> Parser a
disjoint parsers = do
  c <- newCommitment
  r <- foldl1 (<|>) parsers
  c
  return r

{-
try :: Parser a -> Parser (Maybe a)
try parser = (Just `fmap` parser) <|> return Nothing
-}

oneOf :: String -> Parser Char
oneOf opts = do
  Just c <- next
  if c `elem` opts then return c else fail ("Encountered: " ++ [c] ++ "; expected one of " ++ show opts)

char :: Char -> Parser ()
char c = oneOf [c] >> return ()

exactly :: String -> Parser ()
exactly = mapM_ char
