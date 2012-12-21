module TextBuffer where

import Prelude hiding ((!!))

-- todo: replace [a] with SizedList (pair of list and int)
data ListZipper a = ListZipper [a] [a]
  deriving (Eq, Ord, Read, Show)

-- todo: also keep track of the portal row
data Buffer = Buffer (ListZipper String)
  deriving (Eq, Ord, Read, Show)

type FileCoord = (Int, Int)
type PortalCoord = (Int, Int)

type WidthBuffer = (Buffer, Int {- ^ columns -})

portalify :: WidthBuffer -> Int {- ^ portal row -} -> [(Bool, String)]
portalify (Buffer lz, cols) portalRow = drop portalRow ret where
  ret = concatMap (chunk False) (unroll lz)
  chunk ov line =
    case splitAt cols line of
      (_,"") -> [(ov, line)]
      (left, right) -> ((ov, left) : chunk True right)

upward, downward, leftward, rightward :: WidthBuffer -> FileCoord -> FileCoord
upward (Buffer lz, cols) (r,c) = z where
  r' = r - 1
  z = case lz `at` r of
        Nothing -> (size lz - 1, 0)
        Just _ ->
          if c - cols < 0
            then case lz `at` r' of
                   Nothing -> (0, 0)
                   Just line' ->
                     let l = length line'
                         c' = if l < c then l else last [c,(c+cols)..l] in
                     (r', c')
            else (r, c - cols)
downward (Buffer lz, cols) (r,c) = z where
  fileEnd = (size lz - 1, length (final lz))
  r' = r + 1
  z = case lz `at` r of
        Nothing -> fileEnd
        Just line ->
          let l = length line in
          if c + cols <= l
            then (r, c + cols)
            else case lz `at` r' of
                   Nothing -> fileEnd
                   Just line' -> (r', min (length line') c)
leftward _ (0,0) = (0,0)
leftward (Buffer lz, _) (r,0) =
  let r' = r - 1 in
  case lz `at` r' of
    Nothing -> error $ "no line found at " ++ show r'
    Just line -> (r', length line)
leftward _ (r,c) = (r, c - 1)
rightward (Buffer lz, _) (r,c) =
  case lz `at` r of
    Nothing -> case size lz of
                 0 -> (0, 0)
                 s -> (s - 1, length (final lz))
    Just line ->
      if c >= length line
        then if r >= size lz - 1 then (r, c) else (r + 1, 0)
        else (r, c + 1)

translate :: WidthBuffer -> FileCoord -> PortalCoord
translate (Buffer lz, cols) (r, c) = (r' + off, c `mod` cols) where
  r' = sum (map portalLines (take r (unroll lz)))
  portalLines line = 1 + (length line `div` cols)
  off = case lz `at` r of
          Nothing -> 0
          Just _ -> c `div` cols

insert :: Char -> FileCoord -> Buffer -> Buffer
insert char (r, c) (Buffer lz) = Buffer z where
  z = case lz `splitOn` r of
        Nothing ->
          let rlz = reversed lz in
          case char of
            '\n' -> ListZipper ("" : rlz) []
            _ ->
              case rlz of
                [] -> ListZipper [[char]] []
                (x:xs) -> ListZipper ((x ++ [char]) : xs) []
        Just (before, on, after) ->
          let (left, right) = splitAt c on in
          case char of
            '\n' -> ListZipper before (left : right : after)
            _ -> ListZipper before ((left ++ (char : right)) : after)
delete :: FileCoord -> Buffer -> (Buffer, Bool {- ^ changes made -})
delete (r, c) b@(Buffer lz) =
  case lz `splitOn` r of
    Nothing -> (b, False)
    Just (before, on, after) ->
      case splitAt c on of
        (_,"") ->
          case after of
            [] -> (Buffer (ListZipper before [on]), True)
            (x:xs) -> (Buffer (ListZipper before ((on ++ x) : xs)), True)
        (left,(_:right)) ->
          (Buffer (ListZipper before ((left ++ right) : after)), True)

fromString :: String -> Buffer
fromString s = Buffer (ListZipper [] (lines s))

toString :: Buffer -> String
toString (Buffer lz) = unlines (unroll lz) where

unroll (ListZipper before after) = combine before after where
  combine [] as = as
  combine (x:xs) as = combine xs (x:as)

at :: ListZipper a -> Int -> Maybe a
at (ListZipper before after) i = z where
  lb = length before
  z | i < lb = before !! (lb - i - 1)
    | otherwise = after !! (i - lb)

(!!) :: [a] -> Int -> Maybe a
_ !! i | i < 0 = error "negative index on !!"
xs !! i = find i xs where
  find _ [] = Nothing
  find 0 (x:_) = Just x
  find i (_:xs) = find (i - 1) xs

final (ListZipper (x:_) []) = x
final (ListZipper _ xs) = last xs

size (ListZipper b a) = length b + length a

reversed (ListZipper before after) = shift before after where
  shift xs [] = xs
  shift xs (y:ys) = shift (y:xs) ys

-- todo: make faster
splitOn :: ListZipper a -> Int -> Maybe ([a], a, [a])
splitOn _ i | i < 0 = error "cannot splitOn negative"
splitOn lz i =
  case splitAt i (unroll lz) of
    (_, []) -> Nothing
    (bf, (x:xs)) -> Just (reverse bf, x, xs)
