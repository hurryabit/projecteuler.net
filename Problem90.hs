module Problem90 where

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

extend :: [Int] -> [Int]
extend [] = []
extend (6:xs) = 6:9:xs
extend (9:xs) = 6:9:xs
extend (x:xs) = x:extend xs

combine :: [Int] -> [Int] -> [Int]
combine xs ys = comb xs ys ++ comb ys xs
  where comb as bs = [ 10*a+b | a <- as, b <- bs ]

squares :: [Int]
squares = [1,4,9,16,25,36,49,64,81]

good :: [Int] -> [Int] -> Bool
good xs ys = all (`elem` cs) squares
  where cs = combine xs ys

solution = length [ (c1,c2) | c1 <- cs, c2 <- cs, good c1 c2 ] `div` 2
  where cs = map extend $ choose 6 [0..9]
