{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances #-}
module Problem308 where

import Prelude hiding (init)
import Control.Monad.State.Strict
import Data.Maybe

--  0  1  2  3  4  5  6  7  8  9
--  0  1  2  3  A  B  C  D  E  F
--  2  3  5  7 11 13 17 19 23 29

-- 3 B -> C
-- 2 C -> 0 1 B
-- 1 C -> D
-- 0 D -> E
-- 1 A -> F
-- F   -> 4 A
-- E   -> 2 D
-- D   -> 3 A
-- C   ->
-- B   -> A
-- A   -> B
-- 0   -> 1 2
-- 3   ->
--     -> 2 A

(<+>) :: MonadPlus m => m a -> m a -> m a
(<+>) = mplus

type Var s a = (s -> a,s -> a -> s)

getVar :: MonadState s m => Var s a -> m a
getVar (g,_) = liftM g get

putVar :: MonadState s m => Var s a -> a -> m ()
putVar (_,s) x = state (\st -> ((),s st x))

modifyVar :: MonadState s m => Var s a -> (a -> a) -> m ()
modifyVar v f = getVar v >>= putVar v . f

data Tag = O | A | B | C | D
    deriving (Show, Eq, Ord, Enum)

data Memory = Memory
    { _tag  :: !Tag
    , _x0 :: !Int
    , _x1 :: !Int
    , _x2 :: !Int
    , _x3 :: !Int
    , _ctr  :: !Int
    }
    deriving (Show)

tag :: Var Memory Tag
tag = (_tag,\st x -> st { _tag = x })

type Reg = Var Memory Int

x0, x1, x2, x3 :: Reg
x0 = (_x0,\st x -> st { _x0 = x })
x1 = (_x1,\st x -> st { _x1 = x })
x2 = (_x2,\st x -> st { _x2 = x })
x3 = (_x3,\st x -> st { _x3 = x })

ctr :: Var Memory Int
ctr = (_ctr,\st x -> st { _ctr = x})

init :: Memory
init = Memory
    { _tag  = O
    , _x0 = 1
    , _x1 = 0
    , _x2 = 0
    , _x3 = 0
    , _ctr  = 0
    }


type Machine a = StateT Memory Maybe a

class Val v where
    val :: v -> Machine Int

instance Val Int where
    val = return

instance Val Reg where
    val = getVar

inc, dec :: Reg -> Machine ()
inc reg = modifyVar reg succ
dec reg = do
    x <- getVar reg
    guard (x > 0)
    putVar reg (pred x)

del :: Reg -> Machine ()
del reg = putVar reg 0

add, sub :: Val v => Reg -> v -> Machine ()
add x y = val y >>= modifyVar x . (+)
sub x y = do
    w <- val y
    modifyVar x (\v -> v-w)

goto :: Tag -> Machine ()
goto l = tic (1 :: Int) >> putVar tag l

loc :: Machine Tag
loc = getVar tag

tic :: Val v => v -> Machine ()
tic = add ctr

step :: Memory -> Memory
step = fromJust . execStateT stepM
    where
    stepM = do
        l <- loc
        case l of
            O -> do
                tic x0
                add x1 x0
                add x2 x0
                del x0
                tic x3
                del x3
                inc x2
                goto A
            A -> do
                tic x1
                tic x1
                tic (1::Int)
                add x3 x1
                del x1
                v2 <- val x2
                v3 <- val x3
                if v2 >= v3 then do
                    tic x3
                    tic x3
                    sub x2 x3
                    add x0 x3
                    add x1 x3
                    del x3
                    goto A
                else do
                    tic x2
                    tic x2
                    tic (1 :: Int)
                    sub x3 x2
                    dec x3
                    add x0 x2
                    add x1 x2
                    del x2
                    v1 <- val x1
                    if v1 > 0 then do
                        tic x0
                        tic x0
                        tic (1::Int)
                        dec x1
                        add x2 x0
                        del x0
                        inc x3
                        goto A
                    else
                        goto O

prime :: Memory -> Bool
prime mem = case mem of
    Memory O _ 0 0 0 _ -> True
    _                  -> False

count = map _ctr $ filter prime $ iterate step init
