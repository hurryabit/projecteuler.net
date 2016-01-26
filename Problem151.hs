module Problem151 where

import Data.Map (fromListWith, toList)
import Data.Ratio

type Config = [Integer]
type Prob = Rational

mapFst :: (a -> a') -> (a,b) -> (a',b)
mapFst f (x,y) = (f x,y)

mapSnd :: (b -> b') -> (a,b) -> (a,b')
mapSnd g (x,y) = (x,g y)

succs :: Config -> [(Config,Prob)]
succs c = run c
  where s = sum c
        run [] = []
        run (x:xs) = (if x==0 then id else (((x-1):map (+1) xs,x%s):)) $
                       map (mapFst (x:)) (run xs)

step :: [(Config,Prob)] -> [(Config,Prob)]
step = toList . fromListWith (+) .
  concatMap (\(c,p) -> map (mapSnd (*p)) (succs c))

steps :: [[(Config,Prob)]]
steps = take 14 $ iterate step [([1,1,1,1],1)]

isGood :: Config -> Bool
isGood c = 1 == sum c

solution = sum . map snd . filter (isGood . fst) . concat $ steps
