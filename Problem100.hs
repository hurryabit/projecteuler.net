module Problem100 where

blues :: [Integer]
blues = 3:15:zipWith (-) (map (\k -> 6*k-2) (tail blues)) blues
