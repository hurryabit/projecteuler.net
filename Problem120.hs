module Problem120 where

sol = sum $ [ a*a-a | a <- [3,5..1000] ] ++ [ a*a-2*a | a <- [4,6..1000] ]
