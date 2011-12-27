module IDE.Undefineditor.Interface.Stateful (
  srcPath
) where

srcPath :: FilePath -> Stream [FilePath] -> LibStream [FilePath]
srcPath fp known lib = srcPath (libStreams lib) (libRvars lib) fp known
