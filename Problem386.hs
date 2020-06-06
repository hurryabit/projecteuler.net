import Control.Monad
import Data.Array
import Data.Array.ST
import Data.List
import Data.MemoCombinators
import qualified Data.Vector as Vec
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as MVec

import Numbers

combinations :: Int -> [Int] -> Int
combinations = memo2 bits (list bits) combinations'
  where
    combinations' 0 _      = 1
    combinations' _ []     = 0
    combinations' n (k:ks) = sum [ combinations (n-i) ks | i <- [0 .. min k n] ]

numAntiChains :: [(Int,Int)] -> Int
numAntiChains pks =
  let ks = sort (map snd pks)
  in  combinations (maximum ks) ks

solution n =
  let tab = factorizationArray n
  in  1 + sum (map numAntiChains $ tail $ elems tab)

main1 = print $ sum $ concatMap (map snd) $ elems $ factorizationArray 1000000

main2 = do
  let table = factTable 10000000
      elms  = elems table
  print $ sum $ concatMap (map length . group) $ elms

main = main2

factTable :: Int -> Array Int [Int]
factTable n = runSTArray $ do
  table <- newArray (1,n) []
  forM_ (takeWhile (<= n) $ iterate (+1) 2) $ \p -> do
    pks <- readArray table p
--    case pks of
      -- p is composite
--      _:_ -> writeArray table p (reverse pks)
      -- p is a prime
    when (null pks) $
      forM_ (takeWhile (<= n) $ iterate (*p) p) $ \q ->
        forM_ (takeWhile (<= n) $ iterate (+q) q) $ \m -> do
          updateArray table m (p:)
  return table

updateArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
updateArray tab idx fun = readArray tab idx >>= writeArray tab idx . fun

type Point = [Int]

space :: Point -> [Point]
space [] = [[]]
space (d:ds) = (:) <$> [0 .. d] <*> space ds

incomparable :: Point -> Point -> Bool
incomparable xs ys =
  let cmps = zipWith compare xs ys
  in  GT `elem` cmps && LT `elem` cmps

antichains :: [Point] -> [[Point]]
antichains [] = [[]]
antichains (x:xs) = map (x:) (antichains $ filter (incomparable x) xs) ++ antichains xs
