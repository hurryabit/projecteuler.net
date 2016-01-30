module Problem125 where

import Data.List

solution :: Integer
solution = sum . map head . group . sort . filter (\n -> let s = show n in s == reverse s) . concat . takeWhile (not . null) . map (takeWhile (<100000000) . drop 2 . scanl (\s n -> s+n*n) 0) . tails $ [1..]
