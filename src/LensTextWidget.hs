{-# LANGUAGE TemplateHaskell #-}
module LensTextWidget where

import Control.Lens
import qualified Data.Sequence as S

data FileCoord = FileCoord {
  _fileRow :: Int,
  _fileColumn :: Int }

data PortalCoord = PortalCoord {
  _portalRow :: Int,
  _portalColumn :: Int -- hidden column; visible cursor might be further left if the line is too short
  }

{-
data ViewCoord = ViewCoord {
  _viewRow :: Int,
  _viewColumn :: Int
  } -- view coordinate is just what you get from subtracting the cursor version of UpperLeft from the cursor version position of cursor
  -}

{-
type Columns = Int
type UpperLeft = FileCoord -- where the view starts
type Cursor
-}

data Buffer = Buffer { _seqs :: S.Seq (S.Seq Char) }
  deriving (Eq, Ord, Read, Show)

makeLenses ''Buffer

{-
portal :: Buffer -> Columns -> Simple Iso FileCoord CursorCoord
portal b

render :: Columns -> UpperLeft -> Getter Buffer [(Bool, String)]

-}

-- use this for inserts and deletes
slicedFromCoord :: FileCoord -> Simple Lens Buffer (S.Seq (S.Seq Char))
slicedFromCoord (FileCoord r c) = seqs.(lens get set) where
  parts ss =
    case S.splitAt r ss of
      (bfLine, right) ->
        case S.viewl right of
          S.EmptyL -> error $ "coordinate is past last line: " ++ show r ++ ", " ++ show c
          (x S.:< xs) -> (bfLine, S.splitAt c x, xs)
  get ss = afChar S.<| afLine
    where (_, (_, afChar), afLine) = parts ss
  set ss repl =
    case S.viewl repl of
      S.EmptyL -> error $ "replacement is empty; if you really want to delete to the end, use Data.Sequence.singleton Data.Sequence.empty"
      (x S.:< xs) -> bfLine S.>< ((bfChar S.>< x) S.<| xs)
    where (bfLine, (bfChar, _), _) = parts ss
