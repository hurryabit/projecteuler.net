module Problem172 where

fak17 :: Integer
fak17 = product [1..17]

fak :: Integer -> Integer
fak 0 = 1
fak 1 = 1
fak 2 = 2
fak 3 = 6

possibs :: Int -> Integer -> [[Integer]]
possibs 0 0 = [[]]
possibs 0 _ = []
possibs l s = [ d:ds | d <- [0..min 3 s], ds <- possibs (l-1) (s-d) ]

score :: [Integer] -> Integer
score ds@(d0:_) = ((18-d0)*fak17) `div` product (map fak ds)

solution :: Integer
solution = sum . map score $ possibs 10 18
