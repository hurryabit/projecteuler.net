module Problem185 where

import Data.List
import Data.Ord
import Data.Set (Set)
import qualified  Data.Set as Set

type Pin = Char

pins :: Set Pin
pins = Set.fromDistinctAscList ['0'..'9']

type Pattern = [Bool]

patterns :: Int -> Int -> [Pattern]
patterns k n
    | k == 0    = [replicate n False]
    | k == n    = [replicate k True]
    | otherwise = map (True:) (patterns (k-1) (n-1)) ++ map (False:) (patterns k (n-1))

apply :: [Set Pin] -> [Pin] -> Pattern -> Maybe [Set Pin]
apply [] [] []          = Just []
apply (rs:rss) (g:gs) (True:bs)
    | g `Set.member` rs = fmap (Set.singleton g:) $ apply rss gs bs
    | otherwise         = Nothing
apply (rs:rss) (g:gs) (False:bs)
    | Set.null rs'      = Nothing
    | otherwise         = fmap (rs':) $ apply rss gs bs
    where rs' = g `Set.delete` rs

type Guess = ([Pin],Int)

backtrack :: Int -> [Set Pin] -> [Guess] -> [[Pin]]
backtrack n rss []
    | all ((1 ==) . Set.size) rss = [map Set.findMin rss]
    | otherwise                   = []
backtrack n rss ((ps,c):gss)      = concat [ backtrack n rss' gss | Just rss' <- map (apply rss ps) (patterns c n) ]

--main = putStrLn . head $ backtrack 16 (replicate 16 pins) guesses
main = putStrLn . head $ backtrack 5 (replicate 5 pins) exGuesses

guesses :: [Guess]
guesses = sortBy (comparing snd)
  [("5616185650518293",2)
  ,("3847439647293047",1)
  ,("5855462940810587",3)
  ,("9742855507068353",3)
  ,("4296849643607543",3)
  ,("3174248439465858",1)
  ,("4513559094146117",2)
  ,("7890971548908067",3)
  ,("8157356344118483",1)
  ,("2615250744386899",2)
  ,("8690095851526254",3)
  ,("6375711915077050",1)
  ,("6913859173121360",1)
  ,("6442889055042768",2)
  ,("2321386104303845",0)
  ,("2326509471271448",2)
  ,("5251583379644322",2)
  ,("1748270476758276",3)
  ,("4895722652190306",1)
  ,("3041631117224635",3)
  ,("1841236454324589",3)
  ,("2659862637316867",2)
  ]

exGuesses :: [Guess]
exGuesses = sortBy (comparing snd)
    [("90342",2)
    ,("70794",0)
    ,("39458",2)
    ,("34109",1)
    ,("51545",2)
    ,("12531",1)
    ]