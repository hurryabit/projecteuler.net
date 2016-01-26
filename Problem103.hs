module Problem103 where

import Control.Monad
import System.Environment
import Problem105 hiding (main)

incSeq :: Int -> Int -> Int -> [[Int]]
incSeq 1 s n
    | s <=n     = [[n]]
    | otherwise = []
incSeq r s n    = [ a:as | a <- [s..n `div` r], as <- incSeq (r-1) a (n-a) ]

main = do
--    [s,n] <- getArgs
    forM_ [ (s,n) | s <- [18,17..0], n <- [254,253..230] ] $ \(s,n) -> do
        putStrLn $ "Processing s=" ++ show s ++ " & n=" ++ show n
        mapM_ print $ filter isSpecial (incSeq 7 s n)