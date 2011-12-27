module IDE.Undefineditor.Util.RefCountMap (
  RefCountMap(),
  empty,
  increment,
  decrement
) where

import qualified Data.Map as M

newtype RefCountMap k v = RefCountMap (M.Map k (Int, v))

empty :: RefCountMap k v
empty = RefCountMap M.empty

increment :: (Monad m, Ord k) => k -> m v -> RefCountMap k v -> m (Int, v, RefCountMap k v)
increment k v (RefCountMap m) =
  case M.lookup k m of
    Nothing -> do
      z <- v
      return (1, z, RefCountMap (M.insert k (1, z) m))
    Just (i, z) -> return (i + 1, z, RefCountMap (M.insert k (i + 1, z) m))

decrement :: (Ord k) => k -> RefCountMap k v -> (Int, v, RefCountMap k v)
decrement k (RefCountMap m) = down (M.lookup k m) where
  down Nothing = error "decremented past zero"
  down (Just (1, z)) = (0, z, RefCountMap (M.delete k m))
  down (Just (i, z)) = (i - 1, z, RefCountMap (M.insert k (i - 1, z) m))

-- lookup :: (Ord k) => k -> RefCountMap k v -> Maybe (Int, v)
-- lookup k (RefCountMap m) = M.lookup k m
