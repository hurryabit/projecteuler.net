{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, RecordWildCards, ScopedTypeVariables, TypeOperators #-}
import Data.Coerce
import Data.Function
import Data.IntMap.Strict (IntMap)
import Data.List
import Data.Monoid
import Data.Ord
import Data.Set (Set)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

infixr 1 :>

main = print $ volume $ sort $ take 50000 randomCuboids

data InternalCuboid =
  Cuboid
  { x1, y1, z1 :: Int
  , dx, dy, dz :: Int
  }
  deriving (Show, Eq, Ord)

newtype Cuboid x = Wrap { internal :: InternalCuboid }
  deriving (Show, Eq)

class Coordinate x where
  volume  :: [Cuboid x] -> Int
  partialCompare :: Cuboid x -> Cuboid x -> Ordering

class Coordinate x => BaseCoordinate x where
  u1, u2, du :: Cuboid x -> Int
  u2 = (+) <$> u1 <*> du


instance Coordinate x => Ord (Cuboid x) where
  compare a b = partialCompare a b <> comparing internal a b


baseVolume :: BaseCoordinate x => [Cuboid x] -> Int
baseVolume = sum . snd . mapAccumL combine 0
    where
      combine umax cube
        | umax <= u1 cube = (u2 cube, du cube       ) --(--)--[--]--
        | umax <  u2 cube = (u2 cube, u2 cube - umax) --(--[--)--]--
        | otherwise       = (umax   , 0             ) --(--[--]--)--

basePartialCompare :: BaseCoordinate x => Cuboid x -> Cuboid x -> Ordering
basePartialCompare = comparing u1


data X
data Y
data Z

instance Coordinate X where
  volume = baseVolume
  partialCompare = basePartialCompare

instance BaseCoordinate X where
  u1 = x1 . internal
  du = dx . internal

instance Coordinate Y where
  volume = baseVolume
  partialCompare = basePartialCompare

instance BaseCoordinate Y where
  u1 = y1 . internal
  du = dy . internal

instance Coordinate Z where
  volume = baseVolume
  partialCompare = basePartialCompare

instance BaseCoordinate Z where
  u1 = z1 . internal
  du = dz . internal


data a :> b

instance (BaseCoordinate x, Coordinate y) => Coordinate (x :> y) where
  volume = acc_volume . until (IntMap.null . queue) (advance) . initial
  partialCompare a b = partialCompare (coerce a :: Cuboid x) (coerce b) <> partialCompare (coerce a :: Cuboid y) (coerce b)

data Event a = Event { insertions, deletions :: Set a }
  deriving (Show)

instance Ord a => Monoid (Event a) where
  mempty = Event { insertions = Set.empty, deletions = Set.empty }
  evt1 `mappend` evt2 =
    Event
    { insertions = (Set.union `on` insertions) evt1 evt2
    , deletions  = (Set.union `on` deletions ) evt1 evt2
    }

insertEvent :: [Cuboid x] -> Event (Cuboid y)
insertEvent cubes =
  Event { insertions = Set.fromAscList (coerce cubes), deletions = Set.empty }

deleteEvent :: Cuboid x -> Event (Cuboid y)
deleteEvent cube =
  Event { insertions = Set.empty, deletions = Set.singleton (coerce cube) }

data State x y
  = State
  { queue      :: IntMap (Event (Cuboid y))
  , plane      :: Set (Cuboid y)
  , location   :: Int
  , acc_volume :: Int
  }
  deriving (Show)

initial :: forall x y. (BaseCoordinate x, Coordinate y) => [Cuboid (x :> y)] -> State x y
initial cubes =
  let cubes' = coerce cubes :: [Cuboid x]
      queue = IntMap.fromDistinctAscList
                [ (u1 cube, insertEvent grp)
                | grp@(cube:_) <- groupBy ((==) `on` u1) cubes'
                ]
  in  State { plane = Set.empty, location = 0, acc_volume = 0, .. }

advance :: forall x y. (BaseCoordinate x, Coordinate y) => State x y -> State x y
advance state@(State { location = u_old, .. }) = case IntMap.minViewWithKey queue of
  Nothing                      -> state
  Just ((u_new,event),waiting) ->
    let acc_volume' = acc_volume + (u_new - u_old) * volume (Set.toAscList plane)
        plane'      = (plane `Set.difference` deletions event) `Set.union` insertions event
        ins_cubes   = coerce (Set.toList $ insertions event) :: [Cuboid x]
        ins_queue   = IntMap.fromListWith (<>) [ (u2 cube, deleteEvent cube) | cube <- ins_cubes ]
        queue'      = IntMap.unionWith (<>) waiting ins_queue
    in State { queue = queue', plane = plane', acc_volume = acc_volume', location = u_new }


-- projecteuler code
lfg :: [Int]
lfg = [ (100003 - 200003*k + 300007*k^3) `mod` 1000000 | k <- [1 ..55] ] ++
  zipWith (\x y -> (x+y) `mod` 1000000) lfg (drop 31 lfg)

randomCuboids :: [Cuboid (X :> Y :> Z)]
randomCuboids =
  unfoldr
    (\(x1:y1:z1:dx:dy:dz:rest) -> Just (Wrap Cuboid { .. }, rest))
    (zipWith (+) (cycle [0,0,0,1,1,1]) (zipWith mod lfg $ cycle [10000,10000,10000,399,399,399]))

-- test code
cuboid x1 y1 z1 dx dy dz = Wrap Cuboid {..}

sample1 :: [Cuboid (Y :> Z)]
sample1 = sort
  [ cuboid 0 1  1 1 5 5
  , cuboid 0 1  4 1 8 2
  , cuboid 0 4  5 1 1 2
  , cuboid 0 7  5 1 4 3
  , cuboid 0 3  6 1 5 5
  , cuboid 0 3  8 1 2 4
  , cuboid 0 6 10 1 2 4
  , cuboid 0 5 13 1 2 3
  ]

sample2 :: [Cuboid (X :> Y :> Z)]
sample2 = sort $ take 100 randomCuboids