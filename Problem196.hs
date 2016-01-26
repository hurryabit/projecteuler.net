module Problem196 where

import Data.Array
import Data.List
import Data.Maybe
import Data.Numbers.Primes
import Numbers (isPrimeBy)

line :: Integer -> [Integer]
line n = [n*(n-1) `div` 2 + 1.. n*(n+1) `div` 2]

blocks :: Int -> [a] -> [[a]]
blocks r xs = let xss = take r (iterate (drop 1) xs)
              in  zipWith const (transpose xss) (drop r xs)

adjacent :: (Int,Int) -> (Int,Int) -> Bool
adjacent i@(i1,i2) j@(j1,j2) = i /= j && abs (i1-j1) <= 1 && abs (i2-j2) <= 1

goodSquare :: Array (Int,Int) Bool -> Bool
goodSquare sq = sq ! (0,0) &&
    any (\(i,j) -> adjacent (0,0) i && (adjacent i j && (0,0) /= j || adjacent (0,0) j && i /= j) && sq ! i && sq ! j) (range (((-1,-1),(-2,-2)),((1,1),(2,2))))

fun :: Integer -> Integer
--fun 5678027 = 63786821487691201
--fun 7208785 = 2161278284496390359
fun n = let ps = [ False:False:map (isPrimeBy primes) (line (n+fromIntegral i)) ++ replicate (2-i) False | i <- [-2..2] ]
        in  sum . catMaybes $ zipWith (\b k -> if goodSquare . listArray ((-2,-2),(2,2)) . concat $ b then Just k else Nothing) (blocks 5 . transpose $ ps) (line n)

main = print $ fun 5678027 + fun 7208785
--main = print $ fun 200000

322303240771079935