import Data.List (group)
import Data.MemoCombinators (bits)
import Data.Numbers.Primes (primeFactors)


main = print $ evalModRepThr 29 (c $ c $ c $ 10000) (13^8)

c :: NumPlus a => a -> a
c m = 2 ^. (3 ^. (m-2)) * 3 ^. (3 * ((3 ^. (m-3) - 1) /. 2))

-- add a threshold to ModRep at which the representation is
-- automatically changed to that for large numbers
newtype ModRepThr = ModRepThr (Integer -> ModRep)

-- log_2 n is a safe choice for thr
evalModRepThr :: Integer -> ModRepThr -> Integer -> Integer
evalModRepThr thr (ModRepThr f0) n  = evalModRep (f0 thr) n

mkModRepThr :: (Integer -> ModRep) -> ModRepThr
mkModRepThr = ModRepThr . bits

liftThr1 :: (ModRep -> ModRep) -> ModRepThr -> ModRepThr
liftThr1 op (ModRepThr f0) = mkModRepThr $ \thr -> adjust thr (op (f0 thr))

liftThr2 :: (ModRep -> ModRep -> ModRep) -> ModRepThr -> ModRepThr -> ModRepThr
liftThr2 op (ModRepThr f0) (ModRepThr g0) = mkModRepThr $ \thr -> adjust thr (f0 thr `op` g0 thr)

instance Num ModRepThr where
  fromInteger a = mkModRepThr $ \thr -> adjust thr (fromInteger a)
  (+) = liftThr2 (+)
  (*) = liftThr2 (*)
  (-) = liftThr2 (-)
  abs = liftThr1 abs
  signum = liftThr1 abs

instance NumPlus ModRepThr where
  x /. b = liftThr1 (/. b) x
  (^.) = liftThr2 (^.)


-- represents large numbers x by the map \n -> x `mod` n
data ModRep = Small Integer | Large (Integer -> Integer)

instance Show ModRep where
  show (Small a) = "Small " ++ show a
  show (Large a) = "Large <<func>>"

evalModRep :: ModRep -> Integer -> Integer
evalModRep x n = case x of
  Small a -> a
  Large f -> f n

mkSmall :: Integer -> ModRep
mkSmall = Small

mkLarge :: (Integer -> Integer) -> ModRep
mkLarge = Large . bits

asLarge :: ModRep -> ModRep
asLarge (Small a) = Large (mod a)
asLarge x         = x

adjust :: Integer -> ModRep -> ModRep
adjust thr x = case x of
  Small a
    | a >= thr -> asLarge x
  _            -> x

instance Num ModRep where
  fromInteger a
    | a < 0     = error "only non-negative numbers can be represented"
    | otherwise = mkSmall a

  Small a + Small b = mkSmall (a+b)
  Small a + Large g = mkLarge $ \n -> (a   + g n) `mod` n
  Large f + Small b = mkLarge $ \n -> (f n + b  ) `mod` n
  Large f + Large g = mkLarge $ \n -> (f n + g n) `mod` n

  Small a * Small b = mkSmall (a*b)
  Small 0 * Large _ = mkSmall 0
  Small a * Large g = mkLarge $ \n -> (a   * g n) `mod` n
  Large _ * Small 0 = mkSmall 0
  Large f * Small b = mkLarge $ \n -> (f n * b  ) `mod` n
  Large f * Large g = mkLarge $ \n -> (f n * g n) `mod` n

  Small a - Small b
      | a < b       = error "minuend cannot be smaller than subtrahend"
      | otherwise   = mkSmall (a-b)
  Large f - Small b = mkLarge $ \n -> (f n - b  ) `mod` n
  _       - Large _ = error "subtrahend cannot be large"

  abs = id

  signum (Small 0) = mkSmall 0
  signum _         = mkSmall 1

instance NumPlus ModRep where
  Small a /. b = mkSmall (a `div` b)
  Large f /. b = chinese $ \p k ->
    let n = p^k
        -- split b into m*c such that m is power of p and c is no more divisible by p
        (m,c):_ = dropWhile (\(_,y) -> y `mod` p == 0) $ iterate (\(x,y) -> (p*x,y `div` p)) (1,b)
        -- invert c modulo n
        (1,c',_) = euclid (c `mod` n) n      
    in  (f (n * m) `div` m * c' `mod` n) `mod` n
    
  x       ^. Small 0 = mkSmall 1
  Small a ^. Small b = mkSmall (a ^ b)
  Large f ^. Small b = chinese $ \p k ->
    let n = p^k
        -- split f a into p^l*a such that a is no more divisible by p
        (l,a):_ = dropWhile (\(_,y) -> y `mod` p == 0) $ iterate (\(i,y) -> (i+1,y `div` p)) (0,f n)
    in  if l*b >= k then 0 else p^(l*b) * evalModRep (asLarge (mkSmall a) ^ b) n `mod` n
  x       ^. Large g =
    let x'@(Large f) = asLarge x
    in  chinese $ \p k ->
          let n   = p^k
          in  if f n `mod` p == 0 then 0 else evalModRep (x' ^ g (phi n)) n


class Num a => NumPlus a where
  (/.) :: a -> Integer -> a
  (^.) :: a -> a -> a


-- factorization
factorize :: Integer -> [(Integer,Integer)]
factorize = map ((,) <$> head <*> (toInteger . length)) . group . primeFactors

-- Euler's totient function
phi :: Integer -> Integer
phi n = product [ (p-1) * p^(k-1) | (p,k) <- factorize n ]

-- Euclid's extended algorithm
euclid :: Integer -> Integer -> (Integer,Integer,Integer)
euclid a b
  | c == 0    = (b,0,1)
  | otherwise = let (d,r,s) = euclid b c
                in  (d,s,r-s*q)
  where (q,c) = a `divMod` b

-- Chinese remainder theorem applied
chinese :: (Integer -> Integer -> Integer) -> ModRep
chinese base = mkLarge $ \n -> fst $ foldl combine (0,1) [ (base p k,p^k) | (p,k) <- factorize n ]
  where
    combine (a,m) (b,n) =
      let (1,r,s) = euclid m n
      in  ((s*n*a+r*m*b) `mod` (m*n),m*n)
