module Problem238 where

import Data.Char (ord)

randomBBS :: [Int]
randomBBS = let rs = 14025256:map (\r -> (r*r) `mod` 20300713) rs in rs

randomD :: [Int]
randomD = map (\c -> ord c - ord '0') . filter ('0' /=) . concatMap show $ randomBBS

{-psums :: String -> [[Int]]
psums (d:ds) = let x = digitToInt d
                   xs:xss = psums ds
               in  case x of
                       0 -> xs:xs:xss
                       _ -> (x:map (x+) xs):xs:xss

numbers :: [[Int]]
numbers = psums digits

merge2 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
merge2 xas@((x,a):xas') ybs@((y,b):ybs') = case x `compare` y of
    LT -> (x,a):merge2 xas' ybs
    EQ -> (x,a):merge2 xas' ybs'
    GT -> (y,b):merge2 xas ybs'

merge :: Int -> [[(Int,Int)]] -> [(Int,Int)]
merge t (xas@((x,a):xas'):xass) = case t `compare` x of
    LT -> merge2 xas $ merge t xass
    EQ -> (x,a):merge (t+1) (xas':xass)
    GT -> merge t (xas':xass)

solution :: [(Int,Int)]
solution = merge 1 $ zipWith (\k -> map (\s -> (s,k))) [1..] (psums digits)

main = print $ sum $ map snd $ take 2000 solution-}