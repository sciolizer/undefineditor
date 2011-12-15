-- | A Map where one key is marked as the focused one. For example, tabs in a
-- notebook are a map from names to widgets, and one name has focus.
--
-- Eventually this data structure may also track a history of focuses,
-- so that reasonable behavior can be implemented when items are deleted from the map.
module IDE.Undefineditor.Gui.Model.FocusedMap (
  FocusedMap(),

  fromList,
  toAscList,

  empty,

  delete,
  deleteFocus,
  insert,

  lookup,
  locateIndex,

  focusOn,
  getFocus,
  lookupFocus,
  setFocus,
  size
) where

import Prelude hiding (lookup)

import Data.List (findIndex)
import qualified Data.Map as M (Map(), delete, empty, fromList, insert, lookup, null, size, toAscList)

-- todo: keep a history of focus

-- | A map from keys to values, where one of the keys is the focused one.
--
-- Only the empty map has no focus.
data FocusedMap k v = FocusedMap (Maybe Int) (M.Map k v)
  deriving (Eq, Show)

-- | Constructs a 'FocusedMap' where the focus is the first element.
fromList :: (Ord k) => [(k,v)] -> FocusedMap k v
fromList xs = FocusedMap i xs' where
  xs' = M.fromList xs
  i | M.null xs' = Nothing
    | otherwise = Just 0

-- | Lookup a value based off of the key.
lookup :: (Ord k) => k -> FocusedMap k v -> Maybe v
lookup k (FocusedMap _ m) = M.lookup k m

-- | Return map as a list of pairs, ordered by key.
toAscList :: (Ord k) => FocusedMap k v -> [(k,v)]
toAscList (FocusedMap _ xs) = M.toAscList xs

-- | The empty map.
empty :: FocusedMap k v
empty = FocusedMap Nothing M.empty

-- | Find the index of the given key.
locateIndex :: Eq k => k -> FocusedMap k t -> Maybe Int
locateIndex elem (FocusedMap _ fm) = locateIndexMap elem fm

locateIndexMap elem xs = findIndex (\(k,_) -> k == elem) (M.toAscList xs)

-- | Removes a key from the map.
delete :: (Ord k) => k -> FocusedMap k v -> FocusedMap k v
delete _ (FocusedMap Nothing _) = FocusedMap Nothing M.empty
delete elem (FocusedMap (Just i) xs) = FocusedMap i' xs' where
  xs' = M.delete elem xs
  i' | M.null xs' = Nothing
     | otherwise =
         case locateIndexMap elem xs of
           Nothing -> Just i -- no elem actually deleted
           Just j | j > i -> Just i
                  | j < i -> Just $ i - 1
                  | otherwise -> Just $ max (i - 1) 0

-- | Remove the currently focused key from the map. Does nothing to the empty map.
deleteFocus :: (Ord k) => FocusedMap k v -> FocusedMap k v
deleteFocus fs =
  case lookupFocus fs of
    Nothing -> fs
    Just (x,_) -> delete x fs

-- | Inserts the key and value pair into the map.
insert :: (Ord k) => k -> v -> FocusedMap k v -> FocusedMap k v
insert elem v (FocusedMap i xs) = FocusedMap i' xs' where
  xs' = M.insert elem v xs
  i' = case (locateIndexMap elem xs', i) of
         (Nothing, _) -> internalBug "inserted element has no index?!"
         (Just j, Nothing) -> Just j
         (Just j, Just k) | j <= k -> Just (k + 1)
                          | otherwise -> Just k

-- | Gets the current focus of the map.
getFocus :: FocusedMap k v -> Maybe Int
getFocus (FocusedMap i _) = i

-- | Returns the key and value of the focused item.
lookupFocus :: (Ord k) => FocusedMap k v -> Maybe (k, v)
lookupFocus (FocusedMap Nothing _) = Nothing
lookupFocus (FocusedMap (Just i) s) = Just $ M.toAscList s !! i

-- | Changes the focused item by key.
focusOn :: (Ord k) => k -> FocusedMap k v -> FocusedMap k v
focusOn elem (FocusedMap i s) = FocusedMap j s where
  j = case locateIndexMap elem s of
        Nothing -> i
        Just k -> Just k

-- | Changes the focused item by index.
setFocus :: Int -> FocusedMap k v -> FocusedMap k v
setFocus i (FocusedMap _ s)
  | M.null s = callerBug "cannot set focus on empty set"
  | i < 0 || i >= M.size s = callerBug "focus out of range"
  | otherwise = FocusedMap (Just i) s

internalBug = error
callerBug = error

-- | Returns the size of the map.
size :: FocusedMap k v -> Int
size (FocusedMap _ s) = M.size s
