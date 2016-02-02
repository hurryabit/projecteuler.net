{-# LANGUAGE NamedFieldPuns, RecordWildCards, TypeOperators #-}
import Data.Function
import Data.IntMap (IntMap)
import Data.List
import Data.Monoid
import Prelude hiding (compare)

import qualified Data.IntMap as IntMap
import qualified Prelude

infixr 1 :>

main = print $ volume (undefined :: X :> Y :> Z) (sort $ take 50000 randomCuboids)

data Cuboid =
  Cuboid
  { x1, y1, z1 :: Int
  , dx, dy, dz :: Int
  }
  deriving (Show, Eq, Ord)

class Coordinate x where
  volume  :: x -> [Cuboid] -> Int
  partialCompare :: x -> Cuboid -> Cuboid -> Ordering

compare :: Coordinate x => x -> Cuboid -> Cuboid -> Ordering
compare x a b = partialCompare x a b <> Prelude.compare a b

class Coordinate x => BaseCoordinate x where
  u1, u2, du :: x -> Cuboid -> Int
  u2 x = (+) <$> u1 x <*> du x


baseVolume :: BaseCoordinate x => x -> [Cuboid] -> Int
baseVolume x = sum . snd . mapAccumL combine 0
    where
      combine umax cube
        | umax <= u1 x cube = (u2 x cube, du x cube       ) --(--)--[--]--
        | umax <  u2 x cube = (u2 x cube, u2 x cube - umax) --(--[--)--]--
        | otherwise         = (umax      , 0              ) --(--[--]--)--

basePartialCompare :: BaseCoordinate x => x -> Cuboid -> Cuboid -> Ordering
basePartialCompare x a b = u1 x a `Prelude.compare` u1 x b --  <> a `Prelude.compare` b

data X
data Y
data Z

instance Coordinate X where
  volume = baseVolume
  partialCompare = basePartialCompare

instance BaseCoordinate X where
  u1 _ = x1
  du _ = dx

instance Coordinate Y where
  volume = baseVolume
  partialCompare = basePartialCompare

instance BaseCoordinate Y where
  u1 _ = y1
  du _ = dy

instance Coordinate Z where
  volume = baseVolume
  partialCompare = basePartialCompare

instance BaseCoordinate Z where
  u1 _ = z1
  du _ = dz


data a :> b = a :> b

instance (BaseCoordinate x, Coordinate y) => Coordinate (x :> y) where
  volume ~(x :> y) = acc_volume . until (IntMap.null . queue) (advance (x :> y)) . initial (x :> y)
  partialCompare ~(x :> y) a b = partialCompare x a b <> partialCompare y a b

data Event a = Insert a | Delete
  deriving (Show)
  
data State
  = State
  { queue      :: IntMap (Event [Cuboid])
  , plane      :: [Cuboid]
  , location   :: Int
  , acc_volume :: Int
  }
  deriving (Show)

initial :: (BaseCoordinate x, Coordinate y) => x :> y -> [Cuboid] -> State
initial ~(x :> y) cubes =
  let queue = IntMap.fromDistinctAscList
                [ (u1 x cube,Insert grp)
                | grp@(cube:_) <- groupBy ((==) `on` u1 x) cubes
                ]
  in  State { plane = [], location = 0, acc_volume = 0, .. }

advance :: (BaseCoordinate x, Coordinate y) => x :> y -> State -> State
advance ~(x :> y) state@(State { location = u_old, .. }) = case IntMap.minViewWithKey queue of
  Nothing                      -> state
  Just ((u_new,event),waiting) ->
    let acc_volume'         = acc_volume + (u_new - u_old) * volume y plane
        clean_plane     = filter ((u_new <) . u2 x) plane
        (queue',plane') = case event of
          Delete       -> (waiting,clean_plane)
          Insert cubes ->
            let del_events = IntMap.fromList [ (u2 x cube,Delete) | cube <- cubes ]
            in  (waiting `IntMap.union` del_events, merge y clean_plane cubes)
    in State { queue = queue', plane = plane', acc_volume = acc_volume', location = u_new }

merge :: Coordinate x => x -> [Cuboid] -> [Cuboid] -> [Cuboid]
merge x []     bs     = bs
merge x as     []     = as
merge x (a:as) (b:bs) = case compare x a b of
  LT -> a :     merge x as     (b:bs)
  EQ -> a : b : merge x as     bs
  GT ->     b : merge x (a:as) bs


-- projecteuler code
lfg :: [Int]
lfg = [ (100003 - 200003*k + 300007*k^3) `mod` 1000000 | k <- [1 ..55] ] ++
  zipWith (\x y -> (x+y) `mod` 1000000) lfg (drop 31 lfg)

randomCuboids :: [Cuboid]
randomCuboids =
  unfoldr
    (\(x1:y1:z1:dx:dy:dz:rest) -> Just (Cuboid { .. }, rest))
    (zipWith (+) (cycle [0,0,0,1,1,1]) (zipWith mod lfg $ cycle [10000,10000,10000,399,399,399]))

-- test code
sample =
  [ Cuboid 0 1  1 1 5 5
  , Cuboid 0 1  4 1 8 2
  , Cuboid 0 4  5 1 1 2
  , Cuboid 0 7  5 1 4 3
  , Cuboid 0 3  6 1 5 5
  , Cuboid 0 3  8 1 2 4
  , Cuboid 0 6 10 1 2 4
  , Cuboid 0 5 13 1 2 3
  ]
