import Data.Monoid
import Data.Ord

data Point = Point { x, y :: Int }
  deriving Eq

instance Ord Point where
  compare a b = foldMap (\sel -> comparing sel a b) [y, x]