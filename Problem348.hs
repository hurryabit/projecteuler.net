import Data.List

import Numbers

merge2D ((x:xs):xxs) = x:merge2D (insert xs xxs)

squbics :: [Int]
squbics = merge2D [ [ x*x+y*y*y | x <- [2 ..] ] | y <- [2 ..] ]

quadrupel :: [a] -> Bool
quadrupel [_,_,_,_] = True
quadrupel _ = False

palindromic :: Int -> Bool
palindromic n =
  let ds = digits n
  in  ds == reverse ds

main = do
  let solution = take 5 $ filter palindromic $ map head $ filter quadrupel $ group squbics
  mapM_ print solution
  print (sum solution)