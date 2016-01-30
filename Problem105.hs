module Problem105 where

import Data.List
import qualified Data.Set as Set

main = do
  sets <- fmap (map (\l -> read $ "[" ++ l ++ "]") . lines) $
            readFile "sets.txt"
  print . sum . map sum . filter isSpecial $ sets

choose :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

unique :: Ord a => [a] -> Bool
unique = and . snd .
  mapAccumL (\set x -> (x `Set.insert` set,x `Set.notMember` set)) Set.empty

isSpecial :: [Int] -> Bool
isSpecial xs =
  let n = length xs
      k = n `div` 2
      (q,r) = (n+1) `divMod` 2
      (ys,zs') = splitAt q (sort xs)
      zs = drop r zs'
  in  unique (map sum $ choose k xs) && sum ys > sum zs
