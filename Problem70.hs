module Problem70 where

import Control.Monad
import Data.Array.IArray
import Data.List
import Data.Ord
import Data.Ratio

import qualified Numbers as N

sol = minimumBy (comparing snd) $ do
  let phiA = N.phiArray 10000000
  (n,phi) <- tail $ assocs phiA
  let rat = fromIntegral n / fromIntegral phi
  guard $ rat < 1.1
  guard $ sort (N.digits n) == sort (N.digits phi)
  return ((n,phi),rat)

main = print sol
