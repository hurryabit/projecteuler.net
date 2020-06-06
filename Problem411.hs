{-# LANGUAGE BangPatterns, DeriveFunctor, TupleSections #-}
import Control.DeepSeq
import Control.Monad
import Data.Function
import Data.List hiding (insert)
import Data.Maybe
import Data.Tuple

import Numbers

infixr 1 `into`


period :: Int -> Int
period n =
  let (n2,k2) = divMaxPow n 2
      (n3,k3) = divMaxPow n 3
      dlog b m = minimum $ filter (\d -> powMod b d m == 1 `mod` m) (divisors (phi m))
  in max k2 k3 + lcm (dlog 2 n2) (dlog 3 n3)

type Point = (Int,Int)

stations :: Int -> Int -> Int -> [Point]
stations a b n = take (min (2*n+1) (period n)) $ zip (powers a) (powers b)
  where powers c = iterate (\i -> c*i `mod` n) 1

initialize :: [(Int,Int)] -> [[Int]]
initialize = map (map snd) . groupBy ((==) `on` fst) . sort

data Tree =
    Node { left , right     :: !Tree
         , label, _maxLabel :: !Int
         , _size            :: !Int
         }
  | Empty
  deriving (Eq)

showLines :: Tree -> [String]
showLines Empty = ["Empty"]
showLines node  =
  let line1 = concat ["Node ", show (label node), " (max=", show (_maxLabel node), ", size=", show (_size node), ")"]
      (line1l:linesl) = showLines (left  node)
      (line1r:linesr) = showLines (right node)
  in  line1:("+-" ++ line1l):map ("| " ++) linesl ++ ("+-" ++ line1r):map ("  " ++) linesr

instance Show Tree where
  show = unlines . showLines

size, maxLabel :: Tree -> Int
size Empty = 0
size node  = _size node
maxLabel Empty = 0
maxLabel node  = _maxLabel node

mkNode :: Tree -> Tree -> Int -> Tree
mkNode l r x = _induceEverything $ Node { left = l, right = r, label = x, _maxLabel = 0, _size = 0 }

mkLeaf :: Int -> Tree
mkLeaf x = mkNode Empty Empty x

_induceEverything, _induceMaxLabel, _induceSize :: Tree -> Tree
_induceEverything    = _induceMaxLabel . _induceSize
_induceMaxLabel node = node { _maxLabel = max (label node) (maxLabel (right node)) }
_induceSize     node = node { _size     = size (left node) + 1 + size (right node) }



adjust :: Int -> Tree -> Tree
adjust x node
  -- x goes into right subtree
  | x >= label node = _induceMaxLabel $ node { right = x `adjust` right node }
  -- x goes into left subtree
  | x < maxLabel (left node) = node { left = x `adjust` left node }
  -- x goes into root
  | otherwise = _induceMaxLabel $ node { label = x }

insert :: Int -> Tree -> Tree
insert x Empty = mkLeaf x
insert x node
  -- x goes into right subtree
  | size (left node) > size (right node) = _induceEverything $ node { right = x `insert` right node }
  -- x becomes new root
  | otherwise = mkNode node Empty x



into :: Int -> Tree -> Tree
into x node
  | x >= maxLabel node = x `insert` node
  | otherwise          = x `adjust` node


treeSolve :: Int -> Int
treeSolve n =
  let init23 = initialize $ stations 2 3 n
      init32 = initialize $ stations 3 2 n
      initab = if length init23 < length init32 then init23 else init32
  in  size $ foldl (flip bulkInto) Empty init23





main = do
    let k = 17
--  sols <- forM [1 .. 30] $ \k -> do
    let s = treeSolve (k^5)
    putStrLn $ "S(" ++ show k ++ "^5) = " ++ show s
--    return s
--  print $ sum sols

{- TIMINGS
      result | select | merge  |  tree  |  bulk  | intmap | lndsub |
 9 ->  39366 |  0.2 s |  0.2 s |  0.1 s |  0.1 s |  0.1 s |        |
16 -> 262144 |  2.5 s |  2.1 s |  1.4 s |  1.2 s |  1.3 s |  1.2 s |
17 ->   2319 | 67   s |  ---   |  8.5 s |  7.5 s |  9.0 s |  7.6 s |
19 ->        |  ---   |        | 18.8 s | 14.8 s | 19.3 s | 15.1 s |

-}

newtype Stream i a = Stream { runStream :: [i] -> (a,[i]) }
  deriving Functor

evalStream :: Stream i a -> [i] -> a
evalStream act = fst . runStream act

instance Applicative (Stream i) where
  pure = return
  (<*>) = ap

instance Monad (Stream i) where
  return x = Stream { runStream = (,) x }
  ma >>= f = Stream { runStream = \is -> let (x,is') = runStream ma is in runStream (f x) is' }

more :: Stream i Bool
more = Stream { runStream = \is -> (not (null is),is) }

peek :: Stream i i
peek = Stream { runStream = \(i:is) -> (i,is) }

ifMore :: a -> Stream i a -> Stream i a
ifMore x act = do
  m <- more
  if m then act else return x

buildTreeS :: Int -> Stream Int Tree
buildTreeS 0 = return Empty
buildTreeS n = do
  let m = n `div` 2
  l <- buildTreeS m
  ifMore l $ do
    x <- peek
    r <- buildTreeS (n-1-m)
    return $ mkNode l r x

fillRightS :: Tree -> Stream Int Tree
fillRightS node
  | sizeR == sizeL = return node
  | sizeR == 0     = do
      r <- buildTreeS sizeL
      return $ _induceEverything $ node { right = r }
  | otherwise      = ifMore node $ do
      x <- peek
      fillRightS $ x `insert` node
  where
    sizeR = size (right node)
    sizeL = size (left  node)

bulkInsertS :: Tree -> Stream Int Tree
bulkInsertS node = do
  node' <- fillRightS node
  ifMore node' $ do
    x <- peek
    bulkInsertS (x `insert` node')

bulkInsert :: [Int] -> Tree -> Tree
bulkInsert xs node = evalStream (bulkInsertS node) xs

bulkInto :: [Int] -> Tree -> Tree
bulkInto []     node = node
bulkInto (x:xs) node
  | x >= maxLabel node = foldl (flip into) node (x:xs) -- `bulkInsert` node
  | otherwise          = xs `bulkInto` (x `adjust` node)
