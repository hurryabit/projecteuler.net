module Problem339 where

import Control.Monad
import Control.Monad.Loops
import Data.Array
import Data.Array.IO
import Data.Array.ST
import Data.List
import Data.Ratio
import Data.IORef


thomas :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
thomas as bs cs ds =
    let cs' = zipWith4 (\a b c c' -> c / (b - c'*a)) (0:as) bs cs (0:cs')
        ds' = zipWith5 (\a b c' d d' -> (d - d'*a) / (b - c'*a)) (0:as) bs (0:cs') ds (0:ds')
        xs  = zipWith3 (\c' d' x -> d' - c'*x) (0:reverse cs') (reverse ds') (0:xs)
    in  reverse xs


expect ::  Double -> Int -> Int -> Double
expect d0 k l =
    let k' = fromIntegral k
        l' = fromIntegral l
        n' = k' + l'
        as = [ -i/n' | i <- [l',l'-1 .. 0] ]
        bs = replicate (l+2) 1
        cs = 0:[ -i/n' | i <- [k' .. n'-1] ]
        ds = d0:replicate l 0 ++ [n']
    in  thomas as bs cs ds !! 1

expect' :: Double -> Int -> Int -> Double
expect' d0 k l =
    let n = k+l
        n_ = fromIntegral n
        cdArr' = listArray (k-1,n-1) $ (0,d0):cds'
        cds' = do
                i <- [k .. n-1]
                let i_ = fromIntegral i
                    (c',d') = cdArr' ! (i-1)
                    q = c'*(i_-n_) - n_
                return (i_/q,d'*(i_-n_)/q)
    in  foldl' (\x i -> let (c',d') = cdArr' ! (i-1) in d' - c'*x) n_ [n,n-1 .. k+1]


expect'' :: Double -> Int -> Int -> Double
expect'' d0 k l =
    let n = k+l
        n_ = fromIntegral n
        cdArr' = runSTArray $ do
                    arr <- newArray_ (k-1,n-1)
                    writeArray arr (k-1) (0,d0)
                    forM_ [k .. n-1] $ \i -> do
                        (c',d') <- readArray arr (i-1)
                        let i_ = fromIntegral i
                            q = c'*(i_-n_) - n_
                        writeArray arr i (i_/q,d'*(i_-n_)/q)
                    return arr
    in  foldl' (\x i -> let (c',d') = cdArr' ! (i-1) in d' - c'*x) n_ [n,n-1 .. k+1]

probs = 0:scanl (\d0 k -> expect'' d0 (k+1) k) 1 [1..]

solve n = (probs !! (n-1) + expect'' (probs !! n) (n+1) (n-1)) / 2

main = print $ solve 4000




-----------------
-- EXPERIMENTS --
-----------------

data State a = Sure a Bool | Open a a a
    deriving (Show, Eq)

instance Functor State where
    fmap f (Sure v k)   = Sure (f v) k
    fmap f (Open l h e) = Open (f l) (f h) (f e)

low, hgh :: Ord a => State a -> a
low (Sure v _)   = v
low (Open l _ e) = max l e
hgh (Sure v _)   = v
hgh (Open _ h e) = h

veryOpen :: Ord a => State a -> Bool
veryOpen (Sure _ _)   = False
veryOpen (Open l h e) = l < e && e < h

isOpen :: State a -> Bool
isOpen (Sure _ _)   = False
isOpen (Open _ _ _) = True

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ x = return x
iterateM n f x = f x >>= iterateM (n-1) f

restrictProbs :: [State Double] -> IO [State Double]
restrictProbs exps' = do
    let n = toInteger $ length exps'
    states <- newListArray (0,n) $ (Sure 0 True):map (Open 0 (fromIntegral n) . low) (tail exps') ++ [Sure (fromIntegral n) False] :: IO (IOArray Integer (State Double))
    whileM_ (liftM (any isOpen) (getElems states)) $ do
--    replicateM_ 20 $ do
--        getElems states >>= print . map (fmap (fromRational :: Rational -> Double))
        forM_ [1..n-1] $ \k -> do
            cur <- readArray states k
            case cur of
                Sure _ _ -> return ()
                Open l h e -> do
                    prv <- readArray states (k-1)
                    nxt <- readArray states (k+1)
                    let l' = max l $ (fromIntegral k * low nxt + fromIntegral (n-k) * low prv) / fromIntegral n
                        h' = min h $ (fromIntegral k * hgh nxt + fromIntegral (n-k) * hgh prv) / fromIntegral n
                        cur'
                          | h' <  e             = Sure e True
                          | abs (l'-h') < 1e-8  = Sure l' False
                          | otherwise           = Open l' h' e
                    writeArray states k cur'
--    getElems states >>= print
    getElems states
