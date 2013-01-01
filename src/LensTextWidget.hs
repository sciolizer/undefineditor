{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module LensTextWidget where

import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Sequence ((><), (<|), (|>), ViewL((:<)), ViewR((:>)))
import qualified Data.Sequence as S

data FileCoord = FileCoord {
  _fileRow :: Int,
  _fileCol :: Int }
  deriving (Eq, Ord, Read, Show)

data PortalCoord = PortalCoord {
  _portalRow :: Int,
  _portalCol :: Int
  }

{-
data ViewCoord = ViewCoord {
  _viewRow :: Int,
  _viewColumn :: Int
  } -- view coordinate is just what you get from subtracting the cursor version of UpperLeft from the cursor version position of cursor
  -}

type Columns = Int
{-
type UpperLeft = FileCoord -- where the view starts
type Cursor
-}

-- should always be non-empty
type Buffer = SS Char

isContinuedLine :: Buffer -> Columns -> PortalCoord -> Bool
isContinuedLine = undefined

portal :: Buffer -> Columns -> Simple Iso FileCoord PortalCoord
portal ss cols = iso toPortal fromPortal where
  lineLength = (+1) . (`div` cols) . S.length
  toPortal (FileCoord r c) = PortalCoord (r' + offset) c' where
    Sum r' = foldMap (Sum . lineLength) (S.take r ss) -- todo: find a cheaper way to do this
    (offset, c') = (c `div` cols, c `mod` cols)
  fromPortal (PortalCoord r c) = FileCoord r' c' where
    lineLengths = S.takeWhileL (<=r) . S.scanl (\s l -> s + lineLength l) 0 $ ss
    r' = S.length lineLengths
    c' = c + cols * (r - l)
    l = case S.viewr lineLengths of
           S.EmptyR -> 0
           _ :> l -> l

normalize :: Buffer -> FileCoord -> FileCoord
normalize = undefined -- or whatever a partially applied set operation is
{-
render :: Columns -> UpperLeft -> Getter Buffer [(Bool, String)]

-}

-- concatenated :: Simple Iso (SS a) (S a)
concatenated = iso combine split where
  combine ss =
    case S.viewr . join . fmap (|> '\n') $ ss of
      S.EmptyR -> internalBug "outer sequence of buffer should never be empty"
      ret :> _ -> ret
  split = ensureNonEmpty . S.unfoldr chunk
  chunk rest =
    if S.null rest then Nothing else
    case S.spanl (/= '\n') rest of
      (left, right) ->
        case S.viewl right of
          ('\n' :< r) -> Just (left, r)
          (_ :< _) -> internalBug "impossible: front should have been \\n"
          S.EmptyL -> Just (left, S.empty)
  ensureNonEmpty x =
    case S.viewl x of
      S.EmptyL -> S.singleton S.empty
      _ :< _ -> x

internalBug = error

type S = S.Seq
type SS a = S.Seq (S.Seq a)

type Slice = (Buffer, FileCoord {- end -})

-- use this for inserts and deletes
-- start and end should be anchored file coordinates
slice :: FileCoord {- start -} -> Simple Lens Slice (S Char)
slice fc1 = lens get set . concatenated where
  parts :: SS a -> FileCoord -> (SS a, (S a, S a), SS a)
  parts ss (FileCoord r c) =
    case S.splitAt r ss of
      (bfLine, right) ->
        case S.viewl right of
          S.EmptyL -> error $ "coordinate is past last line: " ++ show r ++ ", " ++ show c
          (x :< xs) -> (bfLine, S.splitAt c x, xs)
  splitRange :: FileCoord {- end -} -> SS a -> (SS a, (S a, S a), SS a, (S a, S a), SS a)
  splitRange fc2 ss = (left, (leftFst, leftSnd), middle, (rightFst, rightSnd), right) where
    (front, (rightFst, rightSnd), right) = parts ss fc2
    (left, (leftFst, leftSnd), middle) = parts front fc1
  get :: (SS a, FileCoord) -> SS a
  get (ss, fc2) = (leftSnd <| middle) |> rightFst where
    (_, (_, leftSnd), middle, (rightFst, _), _) = splitRange fc2 ss
  set :: (SS a, FileCoord) -> SS a -> (SS a, FileCoord)
  set (ss, fc2) repl =
    case S.viewl repl of
      S.EmptyL -> internalBug $ "replacement is empty; should be at least a singleton of empty; probably a bug in `concatenated`"
      (x :< xs) -> let
        (middle, fc2') =
          case S.viewr xs of
            S.EmptyR -> (S.singleton $ leftFst >< x >< rightSnd, FileCoord (_fileRow fc1) (S.length x + _fileCol fc1))
            (ys :> y) -> (((leftFst >< x) <| ys) |> (y >< rightSnd), FileCoord (_fileRow fc1 + S.length ys + 1) (S.length y)) in
        (left >< middle >< right, fc2') where
          (left, (leftFst, _), _, (_, rightSnd), right) = splitRange fc2 ss
