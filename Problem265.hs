module Problem265 where

import Control.Monad
import Data.Bits
import Data.Word

size, bits :: Int
size = 5
bits = 2^size

result :: Integer
result = sum $ map (fromIntegral . foldl (\n b -> 2*n+b) 0) solutions

solutions = map (\bs -> replicate size 0 ++ 1:bs ++ [1]) $
  go 1 (bit 0 .|. bit 1 .|. bit (bits `div` 2)) (bits - size - 2)
  where go :: Int -> Word32 -> Int -> [[Int]]
        go n set 0 = guard (all (not . testBit set) ns) >> return []
          where ns =  take (size-1) . iterate (\k -> (2*k) `mod` bits) $ (2*n+1) `mod` bits
        go n set rest = do
          b <- [0,1]
          let n' = (2*n+b) `mod` bits
          guard $ not (testBit set n')
          map (b:) $ go n' (setBit set n') (rest - 1)
