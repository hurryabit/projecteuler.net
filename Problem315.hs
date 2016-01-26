module Problem315 where

import Control.Monad
import Data.Array
import Numbers

segments :: [[Bool]]
segments = map (map (=='1'))
    ["1110111", "0010010", "1011101", "1011011", "0111010", "1101011", "1101111", "1110010", "1111111", "1111011"]

basicSavings :: Array (Int,Int) Int
basicSavings = listArray ((0,0),(9,9)) $ liftM2 (\x y -> length . filter (uncurry (&&)) $ zip x y) segments segments

digitalRoot :: Int -> [[Int]]
digitalRoot n
    | n < 10    = [[n]]
    | otherwise = let ds = digits n in ds:digitalRoot (sum ds)

savings :: Int -> Int
savings n = let dr = digitalRoot n
            in  2 * sum (zipWith (\ds es -> sum $ zipWith (\d e -> basicSavings ! (d,e)) ds es) dr (tail dr))

main = print $ sum . map savings . takeWhile (< 20000000) . dropWhile (< 10000000) $ primes
