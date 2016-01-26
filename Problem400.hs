{-# LANGUAGE Rank2Types #-}
module Problem400 where

import Control.Applicative ((<$>),(<*>),pure)
import Data.Foldable (Foldable (..))
import Data.MemoCombinators
import Data.Traversable

data Tree a
    = Node  { _left   :: Tree a
            , _value  :: a
            , _right  :: Tree a
            , _height :: Int
            , _size   :: Int
            }
    | Empty { _height :: Int
            , _size   :: Int
            }
    deriving (Eq, Show)

empty :: Tree a
empty = Empty 0 1

node :: Tree a -> a -> Tree a -> Tree a
node l v r = Node l v r (1 + max (_height l) (_height r)) (_size l + 1 + _size r)

nodeL :: Tree a -> a -> Tree a -> Tree a
nodeL l v r
    | (_size l,_height l) >= (_size r,_height r) = node l v r
    | otherwise                                  = node r v l

instance Functor Tree where
    fmap = fmapDefault
    --fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
    --fmap f Empty        = Empty

instance Foldable Tree where
    foldMap = foldMapDefault
    --foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r
    --foldMap f Empty        = mempty

instance Traversable Tree where
    --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse f (Node l v r h s) = Node <$> traverse f l <*> f v <*> traverse f r <*> pure h <*> pure s
    traverse f (Empty h s)      = pure $ Empty h s


memoTree :: Memo a -> Memo (Tree a)
memoTree m f = table (f empty) (memo3 m (memoTree m) (memoTree m) (\v l r -> f $ node l v r))
    where
    table fempty fnode (Empty _ _)      = fempty
    table fempty fnode (Node l v r _ _) = fnode v l r

fibTrees :: [Tree ()]
fibTrees = let ts = empty:(node empty () empty):zipWith (\l r -> node l () r) (tail ts) ts in ts


moves :: Tree a -> [Tree a]
moves (Empty _ _)      = []
moves (Node l v r _ _) = let ls = moves l
                             rs = moves r
                         in  map (\l' -> nodeL l' v r) ls ++ empty:map (\r' -> nodeL l v r') rs


loose :: Tree () -> Bool
loose = memoTree unit loose'
    where
    loose' :: Tree () -> Bool
    loose' (Empty _ _)         = False
    loose' t@(Node l _ r _ _) = all (not . loose) (moves t)

main = mapM_ (print . length . filter loose . moves) . take 9 $ fibTrees

isomorphic :: Tree a -> Tree b -> Bool
isomorphic (Empty _ _)          (Empty _ _)          = True
isomorphic (Node l1 _ r1 h1 s1) (Node l2 _ r2 h2 s2) = h1 == h2 && s1 == s2 && ((isomorphic l1 l2 && isomorphic r1 r2) || (isomorphic l1 r2 && isomorphic r1 l2))
isomorphic _                    _                    = False