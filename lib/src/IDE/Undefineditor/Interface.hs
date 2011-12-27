-- | There are mainly two ways to use this library: statefully and stateless.
-- The advantage to the stateful approach is that when relevant files and
-- file contents are modified, the consumer of the library gets notified
-- immediately (by 'react'ing to the 'Stream's returned by various functions
-- in this module).
--
-- The advantage to the stateless approach is, of course, that it's simpler.
--
-- All of the functions in this interface are geared toward stateful usage.
-- However, any of the functions can easily be converted to a stateless
-- usage by using the 'once' and 'withLib' functions. For example, to
-- get the src folder of a file statelessly, you can define a helper
-- function:
--
-- >>> srcPathStateless :: FilePath -> IO [FilePath]
-- >>> srcPathStateless fp = once (srcPath fp)
module Interface (
  srcPath
) where

import IDE.Undefineditor.Lib
import qualified IDE.Undefineditor.Src as S

srcPath :: FilePath -> Stream [FilePath] -> LibStream [FilePath]
srcPath fp knownSrcPaths lib = S.srcPath (libFileStreamMap lib) fp knownSrcPaths
