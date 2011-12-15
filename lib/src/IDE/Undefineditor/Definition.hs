{-# LANGUAGE
 TupleSections
 #-}
module IDE.Undefineditor.Definition (
  CharacterOffset(..),
  definitionCandidates,
  getLexeme
) where

import Control.Monad
import Data.List

type CharacterOffset = Int

-- todo: search in the imported files as well
-- If returns nothing, indicates that cursor is not underneath a valid identifier or operator.
definitionCandidates :: FilePath -> String -> CharacterOffset -> IO (Maybe [(FilePath, CharacterOffset)])
definitionCandidates fp contents co = do
  currentLexeme <- getLexeme contents co
  case currentLexeme of
    Nothing -> return Nothing
    Just (word, _, _) -> return (Just (map (fp,) (findSubstringIndices word contents)))

-- technically this function is pure, but I reserve the right to add
-- side effects later by keeping it in the IO monad.
getLexeme :: String -> CharacterOffset -> IO (Maybe (String, Int, Int))
getLexeme "" _ = return Nothing
getLexeme s co = return ret where -- notReserved . const (front ++ back)) =<< cc where
  (before, inclAfter) = splitAt co s
  cc = charClass (s !! co)
  front = largestMatch . reverse . tails $ before -- todo: this is incredibly inefficient
  back = largestMatch . inits $ inclAfter
  largestMatch = last . takeWhile (all ((== cc) . charClass))
  ret = do -- maybe monad, whee!
    cc
    let word = front ++ back
    guard $ word `notElem` reserved
    return (word, co - length front, co + length back)

charClass x | x `elem` ascSymbol = Just Symbol
            | x `elem` nonSymbol = Just Identifier
            | otherwise = Nothing

-- todo: make faster. Possible solution at 
-- http://twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell
findSubstringIndices :: (Eq a) => [a] -> [a] -> [Int]
findSubstringIndices needle haystack = findIndices (needle `isPrefixOf`) (tails haystack)

ascSymbol = ":!#$%&*+./<=>?@\\^|-~" -- remember : is special

-- note the single quote at the end
nonSymbol = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'" -- remember upper case is special
-- todo: support ALL unicde upper case and lower case

-- todo: include uniSymbol

reserved = ["case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_", "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

-- todo: before implementing the entire "jump to definition candidate" function, first just write the Ctrl+W feature of IntelliJ - highlighting the current lexeme.

data CharClass = Identifier | Symbol
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
