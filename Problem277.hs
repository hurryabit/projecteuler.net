module Problem277 where

data Direction = D | U | S
  deriving (Show)

step :: Direction -> [Integer] -> [Integer]
step D = map (3*)
step U = map (\x -> (3*x-2) `div` 4) . filter (\x -> x `mod` 4 == 2)
step S = map (\x -> (3*x+1) `div` 2) . filter odd

steps :: [Direction] -> [Integer]
steps = foldl (flip step) [1..]

stepSeq = [S,D,U,U,S,D,D,U,D,D,S,S,D,S,D,S,S,D,D,U,D,D,S,S,S,U,D,D,D,U]
