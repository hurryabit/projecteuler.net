module Problem107 where

import Control.Monad
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS (bfs)
import Data.Graph.Inductive.Query.DFS (isConnected)
import Data.Ix
import Data.List
import Data.Maybe
import Data.Ord
import System.Random

type Weight = Int

main = do
  contents <- readFile "network.txt"
  let contents' = "[" ++ map f (init contents) ++ "]"
      wires :: [LEdge Weight]
      wires = map (\((x,y),l) -> (x,y,l)) . sortBy (comparing snd) .
        filter (\((x,y),l) -> x<y && l /= 0). zip (range ((1,1),(40,40))) $
        (read contents')
  print $ sum [ l | (_,_,l) <- wires ]
--  ws <- replicateM 1000 $ do
--          rs <- fmap (map (0/=) . randomRs (0::Int,3)) newStdGen
--          return $ minGraph rs 0 (emptyGraph 40) wires
--  print (minimum (catMaybes ws))
  --print wires
  --writeFile "graph.dot" (graphviz' g)
  where f '-'  = '0'
        f '\n' = ','
        f c    = c

--possibilities :: [LEdge Weight]
--possibilities = [(5,7,11),(1,3,12),(1,2,16),(2,4,17),(4,5,18),(4,6,19),
--  (2,5,20),(1,4,21),(4,7,23),(6,7,27),(3,4,28),(3,6,31)]

isReachable :: Graph gr => gr a b -> Node -> Node -> Bool
isReachable g x y = x `elem` bfs y g

minGraph :: DynGraph gr => [Bool] -> Weight -> gr a Weight ->
  [LEdge Weight] -> Maybe Weight -- (gr a Weight,Weight)
minGraph (r:rs) w g es'
  | w > 2153          = mzero
  | isConnected g     = return w --(g,w)
  | null es'          = mzero
  | isReachable g x y = minGraph (r:rs) w g es
  | r                 = minGraph (r:rs) (w+l) (insEdges [(x,y,l),(y,x,l)] g) es
  | otherwise         = minGraph rs w g es
  where ((x,y,l):es) = es'

weight :: Graph gr => gr a Weight -> Weight
weight g = sum [ l | (x,y,l) <- labEdges g, x < y ]

emptyGraph :: Int -> Gr () Weight
emptyGraph n = mkGraph [ (i,()) | i <- [1..n] ] []
