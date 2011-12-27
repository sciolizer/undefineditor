{-# LANGUAGE
 FlexibleContexts,
 FlexibleInstances,
 GeneralizedNewtypeDeriving,
 NoMonomorphismRestriction,
 Rank2Types,
 TupleSections,
 TypeSynonymInstances
 #-}
module IDE.Undefineditor.Src (
  AbsoluteModule,
  Overlay,

  splitAbsoluteModule,
  srcPath

) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import System.FilePath
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances hiding (Error)
import Text.ParserCombinators.UU.Utils

import IDE.Undefineditor.Lib
import IDE.Undefineditor.Util.Reactive
import IDE.Undefineditor.Util.Siphon
import IDE.Undefineditor.Util.Streams

-- todo: switch from String to Text
type Overlay = M.Map FilePath String

type AbsoluteModule = (FilePath {- src folder -}, FilePath {- relative path to file from src folder -}, String {- module name -})

-- Begin with a list of the containing folder and all parent folders.
-- Perform the following reductions on the list of candidates.
--   (once possibilities has been narrowed to a single folder, can jump out early)
-- 1. Filter out all folders that are higher than the cabal file.
--    -- requires stream on all folders
-- 2. Get src folders from the .cabal (if no src folders specified, then assume is the
--    folder containing the .cabal file). Intersect src folders with candidates. If intersection
--    is null, then include the folder of the .cabal file.
--    -- requires a way to spontaneously start new streams (since the .cabal file could change) XXX
-- 3. Intersect candidates with a list of known src folders (which can be derived from other
--    open files, and can be saved from execution to execution). If intersection is null,
--    then keep all.
--    -- requires nothing; stream already given
--    -- have to be careful here... a bad src folder can be launched into perpetuity if the
--       consumer of this library just feeds the results of this function straight back
-- 4. Look at the module name inside of the file. If the module name is not a prefix-free "Main",
--    and the module name is valid, assume src path based off of that.
--    -- requires a stream of the file path given
-- 5. If module name is invalid or "Main" without a prefix, then scan imports, and see if any of
--    them are compatible with the candidate list.
--    -- folder stream from (1) is sufficient
-- 6. If module name is prefix-free "Main", just assume root directory. The reasoning behind this is that
--    a Main which has no reasonable imports is likely just a one-off script.
--    -- no streams required
-- 7. Give up. Return all remaining candidates. User can select the root folder using the UI.
--    -- no streams required

-- lib contains references to the file watcher, and possibly
-- a file system overlay, and certainly a garbage collector
-- which closes connections to open files when they are
-- no longer in use
srcPath :: Streams -> RVars -> FilePath -> Stream [FilePath] -> IO (Stream [FilePath], IO ())
srcPath streams rvars fp known = ret where
  ret = do
    let (_:candidates) = init . map concat . inits . splitPath $ fp
    (folderContents, closers) <- unzip <$> mapM (\c -> dirStream streams c) candidates
    let cabalFPStream = cabalFiles folderContents -- :: Stream [FilePath]
    (cabalContents, closer) <- siphon rvars (==) cabalFPStream readCabalFiles
    return (mergeStreams candidates folderContents cabalContents, closer `mplus` mconcat closers)
  mergeStreams candidates folderContents cabalContents = (`execUniqueT` candidates) $ do
    filterCabalAncestors
    srcFolders
    intersectKnownSrcFolders
    (prefix, moduleName) <- examineModuleName
    examineImports
    when (isNothing prefix && moduleName == "Main") assumeRoot

todo = return ()

filterCabalAncestors :: UniqueT FilePath Stream ()
filterCabalAncestors = todo
srcFolders = todo
intersectKnownSrcFolders = todo
examineModuleName = return (Nothing, "Main") -- todo
examineImports = todo
assumeRoot = todo

data UniqueExit s = FromFail String | FromStored s
instance Error (UniqueExit s) where
  strMsg = FromFail

-- a monad that terminates early when the state becomes a singleton
newtype UniqueT s m a = UniqueT (StateT [s] (ErrorT (UniqueExit s) m) a)
  deriving (Applicative, Functor, Monad)

getCandidates :: (Monad m) => UniqueT s m [s]
getCandidates = UniqueT $ get

putCandidates :: (Monad m) => [s] -> UniqueT s m ()
putCandidates [x] = UniqueT $ lift (throwError (FromStored x))
putCandidates xs = UniqueT $ put xs

execUniqueT :: (Monad m) => UniqueT s m a -> [s] -> m [s]
execUniqueT (UniqueT m) s = do
  v <- runErrorT (runStateT m s)
  case v of
    Left (FromFail s) -> fail s
    Left (FromStored s) -> return [s]
    Right (_, s) -> return s


      -- cabalContents :: Stream [String]
      -- folderContents :: [Stream (Maybe [FilePath])]
      -- should be straightforward to merge these into a stream of .cabal files

readCabalFiles :: [FilePath] -> IO (Stream [String], IO ())
readCabalFiles = undefined

cabalFiles :: [Stream (Maybe [FilePath])] -> Stream [FilePath]
cabalFiles = undefined

-- todo: consider making an AbsoluteFilePath type
-- todo: file paths returned from here are probably relative
--        so they should be joined with the file path of the relevant cabal
cabalSourceFolders :: Maybe String -> [FilePath]
cabalSourceFolders Nothing = []
cabalSourceFolders (Just s) = ret where
  ret = case parsePackageDescription s of
          ParseFailed _ -> []
          ParseOk _ gpd -> nub $ concatMap hsSourceDirs (srcOf1 (condLibrary gpd) ++ map srcOf3 (condTestSuites gpd) ++ map srcOf2 (condExecutables gpd))
  srcOf1 Nothing = []
  srcOf1 (Just ct) = [libBuildInfo . condTreeData $ ct]
  srcOf2 (_, ct) = buildInfo . condTreeData $ ct
  srcOf3 (_, ct) = testBuildInfo . condTreeData $ ct

-- dirStream
--   :: Streams -> FilePath -> STM () -> IO (Stream (Maybe [FilePath]))
--



-- siphon :: RVars -> (a -> IO b) -> Stream a -> IO (Stream b)

-- useful mainly for figuring out the src folder
--
-- A couple of techniques are used to figure out the src folder.
-- 1. Read the file and look at the module name (done). If the module name is Main, look
--    for a prefix in a comment, e.g.
--      module {- IDE.Undefineditor. -} Main where
--    User can also declare explicitly that Main has no prefix by using an empty comment, e.g.
--      module {- -} Main where
-- 2. If the file has no module name, or the name is Main without a comment prefix, then go
--    up the folder hierarchies looking for a .cabal file. Read the .cabal file, and determine
--    what the src folder is. If the .cabal has multiple src folders, skip this step.
-- 3. If there is no .cabal file or the .cabal file is ambiguous, then look at the imports of
--    the original file. Find one which exists relative to this file, and use ITS src folder.
--    If there are multiple correct answers, infer from the module with the most pieces to
--    its name (decreasing the odds that the match is a coincedence).
-- 4. If all else fails, assume the current folder.
splitAbsoluteModule :: FilePath -> Overlay -> IO AbsoluteModule
splitAbsoluteModule = undefined

-- the idea here being: given a file, and we know its location, and we also know its
-- self-representation, we can suggest making a substitution in the file that will
-- make it self consistent. We can also suggest a move operation that will make
-- the file self-consistent.
-- suggestCorrection :: AbsoluteModule??? -> 

-- todo: support lhs syntax

-- finds the module name in the given file if it exists, and a comment prefix (for Main),
-- if it exists.
--
-- If one of the corrections is to insert the "module" symbol, then return Nothing
-- in all other cases, if the "module" symbol exists (i.e. was not inserted), return Just something.
-- parseModuleName :: String -> IO (Maybe (Maybe String, String, Bound))
-- parseModuleName str = undefined -- fst `fmap` runParser moduleNameParser str

moduleNameParser :: Parser (Maybe String, String)
moduleNameParser = pSpaces *> many comment *> pSymbol "module" *> ((\a b -> (strip <$> a, b)) <$> optional comment <*> moduleName)

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse

{- 
commentOrWhitespace :: Parser ()
commentOrWhitespace = (comment <|> whitespace) *> pure ()

commentOrWhitespace = whitespace <|> blockComment <|> lineComment

blockComment = do
  try (string "-")
  -}

-- type Parser a = P (Str Char String LineColPos) a

prefixParser :: Parser String
prefixParser = undefined {-
prefixParser = do
  inlineCommentOpener
  whitespace
  m <- moduleName
  whitespace
  inlineCommentCloser
  return m
  -}

{-
maybeParse :: Parser a -> Parser (Maybe a)
maybeParse = undefined
-}
{-
maybeParse :: Parser a -> Parser (Maybe a)
maybeParse p = (liftM Just p) <|> return Nothing

inlineCommentOpener :: Parser ()
inlineCommentOpener = Parser $ \s ->
  case s of
    ('{':'-':xs) -> Just ((), xs)
    _ -> Nothing

inlineCommentCloser :: Parser ()
inlineCommentCloser = Parser $ \s ->
  case s of
    ('-':'}':xs) -> Just ((), xs)
    _ -> Nothing
    -}

-- todo: check the haskell standard for actually valid names
moduleNameChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789."

moduleName = lexeme $ many (pAnySym moduleNameChars)

keywordModule = undefined
{-
keywordModule = Parser $ \s ->
  case s of
    ('m':'o':'d':'u':'l':'e':xs) -> Just ((), xs)
    _ -> Nothing
    -}

run :: Show t =>  Parser t -> String -> IO ()
run p inp = do  let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
                putStrLn ("--  Result: " ++ show a)
                if null errors then  return ()
                               else  do putStr ("--  Correcting steps: \n")
                                        show_errors errors
                putStrLn "-- "
            where show_errors :: (Show a) => [a] -> IO ()
                  show_errors = sequence_ . (map (putStrLn . show))

-- todo: still need to handle "--" style comments
comment :: Parser String
comment = lexeme $ pToken "{-" *> inComment <* pToken "-}"

inComment :: Parser String
inComment = concat <$> many (notDash <|> notCloser) where
  notDash = ((:[]) <$> pNot 'x' '-')
  notCloser = (\z -> '-':[z]) <$> (pSym '-' *> pNot 'y' '}')

-- pNot :: c -> Parser c
pNot n c = pSatisfy (/= c) (Insertion ("not " ++ show c) n 5)

{-
comment = ret where
  ret = do
    c <- newCommitment
    blockComment c <|> lineComment c
  blockComment c = do
    -}
    -- exactly "{-"
    -- c
    
{-
commentOrWhitespace = Parser $ cow where
  cow ('{':'-':xs) = rest1 xs
  cow ('-':'-':xs) = rest2 xs
  cow xs = whitespace' xs
  rest1 ('-':'}':xs) = cow xs
  rest1 (x:xs) = rest1 xs
  rest1 "" = Just ((), "") -- technically this is a failed parse, but we'll let it slide
  rest2 (x:xs) | x `elem` "\r\n" = cow xs
  rest2 "" = Just ((), "") -- also technically a failed parse, but letting it slide
  -}

{-
whitespace :: Parser ()
whitespace = Parser whitespace'

whitespace' "" = Just ((), "")
whitespace' (x:xs) | x `elem` " \t\n\r" = whitespace' xs
                   | otherwise = Just ((), xs)
                   -}

-- whitespace = many (oneOf " \r\n\t")
whitespace :: Parser String
whitespace = pSpaces

{-
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Monad Parser where
  return x = Parser $ Just . (x,)
  (Parser m) >>= f = Parser $ \s -> case m s of
                                      Nothing -> Nothing
                                      Just (x, t) -> (runParser (f x)) t

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Functor Parser where
  fmap = liftM

instance Alternative Parser where
  empty = (Parser (const Nothing))
  Parser left <|> Parser right = Parser $ \s ->
    case left s of
      Just z -> Just z
      Nothing -> right s
      -}
