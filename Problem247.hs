{-# LANGUAGE BangPatterns #-}
module Problem247 where

import Data.List
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

data Entry = Entry
    { _base  :: !(Double,Double)
    , _index :: !(Int,Int)
    }
    deriving (Show)

data State = State
    { _queue :: PQueue Double Entry
    , _count :: !Int
    , _goods :: !Int
    }
    deriving (Show)

instance (Show k,Show v,Ord k) => Show (PQueue k v) where
    show = show . unfoldr PQ.minViewWithKey

size :: (Double,Double) -> Double
size (x0,y0) = (sqrt ((x0+y0)^2 - 4*(x0*y0-1)) -(x0+y0)) / 2

insertE e = PQ.insert (- size (_base e)) e

start :: State
start =
    let e0 = Entry { _base = (1.0,0.0), _index = (0,0) }
    in  State
        { _queue = insertE e0 PQ.empty
        , _count = 0
        , _goods = 1
        }

isGood (m,n) (Entry { _index = (l,b) }) = l <= m && b <= n

step m (State { _queue = q0, _count = c, _goods = g }) =
    let Just ((t,e0@(Entry { _base = (x,y), _index = (l,b) })),q1) = PQ.minViewWithKey q0
        er = Entry { _base = (x-t,y), _index = (l+1,b) }
        ea = Entry { _base = (x,y-t), _index = (l,b+1) }
    in  State
        { _queue = insertE er $ insertE ea $ q1
        , _count = c+1
        , _goods = g - fromEnum (isGood m e0) + fromEnum (isGood m er) + fromEnum (isGood m ea)
        }

solution m = _count $ until ((0 ==) . _goods) (step m) start

main = print $ solution (3,3)