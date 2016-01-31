import Data.List (group)
import Data.MemoCombinators (bits)
import Data.Numbers.Primes (primeFactors)

main = print $ evalModRep (c $ c $ c $ 10000) (13^8)

c :: ModRep -> ModRep
c m = 2 ^. (3 ^. (m-2)) * 3 ^. (3 * ((3 ^. (m-3) - 1) /. 2))

-- represents a number x by the map \n -> x `mod` n
newtype ModRep = ModRep { evalModRep :: Integer -> Integer }

-- create a representation which uses memoization
mkModRep :: (Integer -> Integer) -> ModRep
mkModRep = ModRep . bits

instance Num ModRep where
  fromInteger a = ModRep $ mod a
  ModRep f + ModRep g = mkModRep $ \n -> (f n + g n) `mod` n
  ModRep f * ModRep g = mkModRep $ \n -> (f n * g n) `mod` n
  ModRep f - ModRep g = mkModRep $ \n -> (f n - g n) `mod` n

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
chinese base = mkModRep $ \n -> fst $ foldl combine (0,1) [ (base p k,p^k) | (p,k) <- factorize n ]
  where
    combine (a,m) (b,n) =
      let (1,r,s) = euclid m n
      in  ((s*n*a+r*m*b) `mod` (m*n),m*n)
    
-- division by a constant
(/.) :: ModRep -> Integer -> ModRep
ModRep f /. b = chinese $ \p k ->
  let n = p^k
      -- split b into m*c such that m is power of p and c is no more divisible by p
      (m,c):_ = dropWhile (\(_,y) -> y `mod` p == 0) $ iterate (\(x,y) -> (p*x,y `div` p)) (1,b)
      -- invert c modulo n
      (1,c',_) = euclid (c `mod` n) n      
  in  (f (n * m) `div` m * c' `mod` n) `mod` n

-- fast exponentiation using Euler's theorem
(^.) :: ModRep -> ModRep -> ModRep
a@(ModRep f) ^. ModRep g = chinese $ \p k ->
  let n   = p^k
  in  if f n `mod` p == 0 then 0 else evalModRep (a ^ g (phi n)) n
