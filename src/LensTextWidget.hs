{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module LensTextWidget where

import Control.Lens
import Control.Monad
import qualified Data.Sequence as S

data FileCoord = FileCoord {
  _fileRow :: Int,
  _fileColumn :: Int }
  deriving (Eq, Ord, Read, Show)

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

-- should always be non-empty
data Buffer = Buffer { _seqs :: S.Seq (S.Seq Char) }
  deriving (Eq, Ord, Read, Show)

makeLenses ''Buffer

{-
portal :: Buffer -> Columns -> Simple Iso FileCoord CursorCoord
portal b

render :: Columns -> UpperLeft -> Getter Buffer [(Bool, String)]

-}

-- concatenated :: Simple Iso (S.Seq (S.Seq a)) (S.Seq a)
concatenated = iso combine split where
  combine ss =
    case S.viewr . join . fmap (S.|> '\n') $ ss of
      S.EmptyR -> internalBug "outer sequence of buffer should never be empty"
      ret S.:> _ -> ret
  split = S.unfoldr chunk
  chunk rest =
    case S.spanl (/= '\n') rest of
      (left, right) ->
        case S.viewl right of
          ('\n' S.:< r) -> Just (left, r)
          (_ S.:< _) -> internalBug "impossible: front should have been \\n"
          S.EmptyL -> Nothing

internalBug = error

-- use this for inserts and deletes
{-
slicedBetween :: FileCoord -> FileCoord -> Simple Lens Buffer (S.Seq Char)
slicedBetween fca fcb = seqs.(lens get set) where
  (fc1, fc2) = if fca < fcb then (fca, fcb) else (fcb, fca)
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
    -}
