import Data.Ratio

es, ps :: [Double]
es = 1:2:(map (2*) $ zipWith (/) ps [1 ..])
ps = scanl1 (+) es

ps' = 1:map (\r -> 2+2*r) rs
hs = map (1/) [1..]
rs = scanl (+) 0 hs