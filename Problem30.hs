module Problem30 where

import Data.Char

isSolution :: Int -> Bool
isSolution n = n == sum (map ((^5) . digitToInt) (show n))
