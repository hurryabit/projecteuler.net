module Problem54 where

import Text.Read

import Poker

main = do
  handPairs <- fmap (map parseHandPair . lines) $ readFile "poker.txt"
  let wins1 = filter (\(h1,h2) -> compareHands h1 h2 == EQ) handPairs
  print $ length wins1
  -- mapM_ print wins1

parseHandPair :: String -> (Hand,Hand)
parseHandPair str = case readPrec_to_S parser 0 str of
  [(hs,"")] -> hs
  where parser = do
          h1  <- readPrec
          ' ' <- get
          h2  <- readPrec
          return (h1,h2)
