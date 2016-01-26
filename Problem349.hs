module Problem349 where

import Control.Monad
import Control.Monad.Loops
import Data.Array.IO
import Data.IORef

type Position = (Int,Int)

type Direction = Int

move (x,y) d = case d of
    0 -> (x,y+1)
    1 -> (x+1,y)
    2 -> (x,y-1)
    3 -> (x-1,y)

rotCW d = (d+1) `mod` 4
rotCC d = (d-1) `mod` 4

data State = State
        { pos :: Position
        , dir :: Direction
        , rnd :: Int
        , blk :: Int
        }
    deriving (Show)

step :: IOUArray Position Bool -> State -> IO State
step brd (State p d r b) = do
    wht <- readArray brd p
    writeArray brd p (not wht)
    let d' = (if wht then rotCW else rotCC) d
        p' = move p d'
        r' = r+1
        b' = if wht then b+1 else b-1
        st' = State p' d' r' b'
    return st'

start :: State
start = State (0,0) 0 0 0

outside :: Int -> State -> Bool
outside off (State (x,y) _ _ _) = abs x >= off || abs y >= off

simulate :: IO [State]
simulate = do
    brd <- newArray ((-200,-200),(200,200)) True
    que <- newIORef []
    iterateUntilM (outside 100) (step brd) start >>=
        iterateUntilM (outside 120) (\st -> modifyIORef que (st:) >> step brd st)
    liftM reverse $ readIORef que