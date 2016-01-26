module Problem280 where

import Control.Applicative
import Data.Bits
import Data.List (partition)
import Data.MemoCombinators
import Numeric.LinearAlgebra.HMatrix    

dim = 5
dimBit = bit dim

data Config = Config
    { _row :: Int
    , _col :: Int
    , _sed :: Bool
    , _bot :: Int
    , _top :: Int
    }
    deriving Show

positions :: [(Int,Int)]
positions = (,) <$> [0 .. dim-1] <*> [0 .. dim-1]

numPositions :: Int
numPositions = dim*dim

next :: Config -> [Config]
next cfg@(Config r c s b t) =
    let cfg' | r == 0     && not s &&      testBit b c  = cfg { _sed = True , _bot = clearBit b c }
             | r == dim-1 &&     s && not (testBit t c) = cfg { _sed = False, _top = setBit   t c }
             | otherwise                                = cfg
    in  [ cfg' { _row = r', _col = c' } | (dr,dc) <- [(-1,0),(1,0),(0,-1),(0,1)]
                                        , let r' = r+dr, 0 <= r' && r' < dim
                                        , let c' = c+dc, 0 <= c' && c' < dim
                                        ]

final :: Config -> Bool
final (Config r c s b t) = r == dim-1 && s && setBit t c == dimBit-1

sameLevel :: Config -> Config -> Bool
sameLevel (Config _ _ s1 b1 t1) (Config _ _ s2 b2 t2) = s1 == s2 && b1 == b2 && t1 == t2

expected :: Config -> Double
expected cfg@(Config _ _ s b t) = expectedV s b t ! packPos cfg

expectedV :: Bool -> Int -> Int -> Vector Double
expectedV = memo3 bool bits bits expectedV'

expectedV' :: Bool -> Int -> Int -> Vector Double
expectedV' s b t
    | t == dimBit-1 = assoc numPositions 0 []
    | otherwise     = let (mA,vb) = unzip $ do
                                      (r,c) <- positions
                                      let cfg = Config r c s b t
                                          nx = next cfg
                                          wt = 1 / fromIntegral (length nx)
                                          (sm,df) = partition (sameLevel cfg) nx
                                          mAi = assoc numPositions 0 $ (packPos cfg,1):[ (packPos cfg',-wt) | cfg' <- sm ]
                                          vbi = 1 + wt * sum (map expected df)
                                      return (mAi,vbi)
                      in  case linearSolve (fromRows mA) (col vb) of
                            Just sol -> head (toColumns sol)
                            Nothing  -> error "expected: no solution"

packPos :: Config -> Int
packPos (Config r c _ _ _) = (dim * r + c)

main = print $ expected (Config (dim `div` 2) (dim `div` 2) False (dimBit-1) 0) - 1

{-pack :: Config -> Int
pack (Config r c s b t) = dimBit * (dimBit * (2 * (dim * r + c) + fromEnum s) + b) + t-}
    
{-unpack :: Int -> Config
unpack x0 = let (x1,r) = x0 `divMod` 5
                (x2,c) = x1 `divMod` 5
                (x3,s) = x2 `divMod` 2
                (x4,b) = x3 `divMod` 32
                (_ ,t) = x4 `divMod` 32
            in  Config r c (s == 1) b t
-}