module Problem213 where

import Control.Monad
import Data.Array
import Data.List
import System.Random

type Field = (Int,Int)

type Distr = Array Field Double

dim = ((0,0),(29,29))

neighbors :: Field -> [Field]
neighbors (x,y) = [ xy' | (xy',c) <- [((x-1,y),x>0),((x+1,y),x<29),((x,y-1),y>0),((x,y+1),y<29)], c ]

numNeighbors :: Field -> Int
numNeighbors (x,y) = 4 - fromEnum (x == 0 || x == 29) - fromEnum (y == 0 || y == 29)

initial :: Field -> Distr
initial xy = accumArray (+) 0.0 dim [(xy,1.0)]

advance :: Distr -> Distr
advance d = array dim [ (xy,f xy) | xy <- range dim ]
    where f xy = sum [ (d ! xy') / (fromIntegral $ numNeighbors xy')| xy' <- neighbors xy ]

final :: Field -> Distr
final xy = foldl (flip ($)) (initial xy) (replicate 50 advance)

expect :: Double
expect = let ds = map final (range dim)
         in  sum [ (if x==y then 4 else 8) * product [ 1 - d ! (x,y) | d <- ds ] | x <- [0..14], y <- [0..x] ]

main = print expect

{-type Distr = Array Int Double

partitions2 :: [a] -> [([a],[a])]
partitions2 [] = [([],[])]
partitions2 (x:xs) = concatMap (\(ys,zs) -> [(x:ys,zs),(ys,x:zs)]) (partitions2 xs)

partitionsN :: Int -> [a] -> [[[a]]]
partitionsN 1 xs = [[xs]]
partitionsN n xs = do
    (ys,zs) <- partitions2 xs
    zss <- partitionsN (n-1) zs
    return $ ys:zss

expect :: Int -> [Distr] -> Double
expect n ds = sum $ do
    dss <- partitionsN n ds
    let mty  = length $ filter null dss
        prob = product $ zipWith (\i ds' -> product [ d ! i | d <- ds' ]) [1..] dss
    return $ fromIntegral mty * prob

approx :: Int -> [Distr] -> Double
approx n ds = sum [ product [ 1 - d ! i | d <- ds ] | i <- [1 .. n] ]

uniform :: Int -> Distr
uniform n = listArray (1,n) (repeat $ 1 / fromIntegral n)

distr :: [Double] -> Distr
distr ps = listArray (1,length ps) ps

genDistr :: Int -> IO Distr
genDistr n = liftM collect $ replicateM (n-1) (randomRIO (0.0,1.0))
    where collect rs = let as = sort rs
                       in  listArray (1,n) $ zipWith (-) (as ++ [1.0]) (0:as)

experiment :: Int -> IO ()
experiment n = do
    ds <- replicateM n (genDistr n)
    print $ expect n ds
    print $ approx n ds-}
