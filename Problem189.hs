module Problem189 where

import Control.Monad
import Data.Function
import Data.List
import qualified Data.Map as Map

main = print . countConfigs $ 3

type Conf = ([Int],[Int],[Int])

configs :: Int -> [(Conf,Integer)]
configs 0 = [ (([c],[c],[c]),1) | c <- [1,2,3] ]
configs n = collect $ do
  let cs = configs (n-1)
  ((l0,r0,b0),n0) <- cs
  let li = reverse l0
      ri = reverse r0
      bi = reverse b0
  ((l1,r1,b1),n1) <- cs
  guard $ b1 `matches` bi
  ((l2,r2,b2),n2) <- cs
  guard $ l2 `matches` li
  ((l3,r3,b3),n3) <- cs
  guard $ r3 `matches` ri
  return ((l3++l1,r1++r2,b2++b3),n0*n1*n2*n3)

countConfigs :: Int -> Integer
countConfigs n =  let l = 2^(n-1)
                      cs = configs (n-1)
                      countMatches s = sum [ n | ((l,_,_),n) <- cs, l `matches` s ]
                      m = Map.fromList . map (\s -> (s,countMatches s)) $ strings (2^(n-1)) [1,2,3]
                      f s = Map.findWithDefault undefined s m
                  in  sum [ n * f l * f r * f b | ((l,r,b),n) <- cs ]

strings :: Int -> [a] -> [[a]]
strings 0 _ = [[]]
strings k as = [ x:xs | xs <- strings (k-1) as, x <- as ]

matches :: Eq a => [a] -> [a] -> Bool
matches cs1 cs2 = and $ zipWith (/=) cs1 cs2

collect :: Ord a => [(a,Integer)] -> [(a,Integer)]
collect = map (\xns@((x,_):_) -> (x,count xns)) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

count :: [(a,Integer)] -> Integer
count = sum . map snd
