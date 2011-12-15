module IDE.Undefineditor.Util.Diff (
  diffAndApply
) where

import qualified Data.IntMap as IM (IntMap(), (!), findMax, fromAscList)
import qualified Data.Map as M (Map(), empty, insert, lookup)

diff
  :: (Eq a)
  => [a] -- ^ old
  -> [a] -- ^ new
  -> [(a, Action)] -- ^ deletes and adds, in that order
diff old new = diff' old new (lcs old new) where
  diff' []        []        [] = []
  diff' (o:os)    n         [] = (o, Delete) : diff' os n []
  diff' o         (n:ns)    [] = (n, Insert) : diff' o ns []
  diff' oo@(o:os) nn@(n:ns) cc@(c:cs)
    | c /= n = (n, Insert) : diff' oo ns cc
    | c /= o = (o, Delete) : diff' os nn cc
    | otherwise = (c, Match) : diff' os ns cs
  diff' _         _         _  = bug "lcs is not common"

data Action = Match | Delete | Insert

toAts :: [(a, Action)] -> [ActionAt a]
toAts acts = toAts' 0 acts where
  toAts' _ [] = []
  toAts' i ((_, Match) : as) = toAts' (i + 1) as
  toAts' i ((_, Delete) : as) = DeleteAt i : toAts' i as
  toAts' i ((a, Insert) : as) = InsertAt i a : toAts' (i + 1) as

data ActionAt a = DeleteAt Int | InsertAt Int a

operate
  :: (Monad m)
  => (Int -> m ())
  -> (Int -> a -> m ())
  -> [ActionAt a]
  -> m ()
operate _ _ [] = return ()
operate remover adder (DeleteAt i : as) =
  remover i >> operate remover adder as
operate remover adder (InsertAt i a : as) =
  adder i a >> operate remover adder as

diffAndApply
  :: (Eq a, Monad m)
  => [a] -- ^ old
  -> [a] -- ^ new
  -> (Int -> m ()) -- ^ remover
  -> (Int -> a -> m ()) -- ^ adder
  -> m ()
diffAndApply old new remover adder =
  operate remover adder (toAts $ diff old new)

data Trim = Both | First | Second | Stop
  deriving (Show)

type Memo = M.Map (Int, Int) (Int, Trim)

mkMap = IM.fromAscList . zip [0..]

start = fst . IM.findMax

lcs :: (Eq a) => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs first second = ret [] fstart sstart where
  fim = mkMap first
  fstart = start fim
  sim = mkMap second
  sstart = start sim
  (m, _) = lcs' fim sim M.empty fstart sstart
  ret acc _f (-1) = acc
  ret acc (-1) _s = acc
  ret acc f s =
    case M.lookup (f, s) m of
      Nothing -> bug "lcs pointed in an invalid direction"
      Just (_, trim) ->
        case trim of
          Stop -> acc -- I don't think this is actually reachable.
          Both -> ret (fim IM.! f : acc) (f - 1) (s - 1) -- todo: add assertion that sim is the same
          First -> ret acc (f - 1) s
          Second -> ret acc f (s - 1)

lcs' :: (Eq a) => IM.IntMap a -> IM.IntMap a -> Memo -> Int -> Int -> (Memo, (Int, Trim))
lcs' _ _ m _f (-1) = (m, (0, Stop))
lcs' _ _ m (-1) _s = (m, (0, Stop))
lcs' first second m f s =
  case M.lookup (f, s) m of
    Just x -> (m, x)
    Nothing ->
      case (first IM.! f, second IM.! s) of
        (x, y) | x == y -> let
                             (m', (bbl, _)) = lcs' first second m (f - 1) (s - 1)
                             ret = (1 + bbl, Both)
                           in (M.insert (f, s) ret m', ret)
               | otherwise ->
                 let
                   (m' , (ffl, _)) = lcs' first second m  (f - 1) s
                   (m'', (ssl, _)) = lcs' first second m' f (s - 1)
                   ret = if ffl > ssl then (ffl, First) else (ssl, Second)
                 in (M.insert (f, s) ret m'', ret)

bug = error
