{-# LANGUAGE TemplateHaskell #-}
module Poker where

import Control.Monad
import Data.DeriveTH
import Data.List (group, intersperse, sortBy)
import Data.Ord (comparing)
import Test.SmallCheck
import Text.Read

import Util

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
  Jack | Queen | King | Ace
  deriving (Bounded, Enum, Eq, Ord)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Bounded, Enum, Eq, Ord)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq)

newtype Hand = Hand [Card]
  deriving (Eq)

data HandRank =
    HighCard
  | OnePair
  | TwoPairs
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
  deriving (Eq, Ord, Show)

hand :: [Card] -> Hand
hand cs
  | length cs == 5 = Hand (sortBy (flip (comparing rank)) cs)
  | otherwise      = error "A hand must consist of 5 cards."

cards :: Hand -> [Card]
cards (Hand cs) = cs

handRank :: Hand -> (HandRank,[Rank])
handRank h@(Hand cs@(c:cs'))
  | flush && straight
      = (if rank c == Ace then RoyalFlush else StraightFlush,[r])
  | flush         
      = (Flush,rs)
  | straight        
      = (Straight,[r])
  | otherwise
      = case sortBy (flip (comparing length)) . group $ rs of
          [[a,_,_,_],[b]]     -> (FourOfAKind,[a,b])
          [[a,_,_],[b,_]]     -> (FullHouse,[a,b])
          [[a,_,_],[b],[c]]   -> (ThreeOfAKind,[a,b,c])
          [[a,_],[b,_],[c]]   -> (TwoPairs,[a,b,c])
          [[a,_],[b],[c],[d]] -> (OnePair,[a,b,c,d])
          _                   -> (HighCard,rs)
  where flush     = all (\c' -> suit c == suit c') cs'
        straight  = and $ zipWith isSucc rs (tail rs)
        rs@(r:_)  = map rank cs

compareHands :: Hand -> Hand -> Ordering
compareHands = comparing handRank

instance Show Rank where
  show rk | rk < Ten= show (fromEnum rk+2)
  show Ten          = "T"
  show Jack         = "J"
  show Queen        = "Q"
  show King         = "K"
  show Ace          = "A"

instance Show Suit where
  show Clubs    = "C"
  show Diamonds = "D"
  show Hearts   = "H"
  show Spades   = "S"

instance Show Card where
  show (Card { rank = rk, suit = st }) = show rk ++ show st

instance Show Hand where
  show (Hand cs) = unwords (map show cs)

instance Read Rank where
  readPrec = do
    c <- get
    case c of
      'T' ->  return Ten
      'J' ->  return Jack
      'Q' ->  return Queen
      'K' ->  return King
      'A' ->  return Ace
      _   ->  do  guard ('2' <= c && c <= '9')
                  return $ toEnum (fromEnum c - fromEnum '2')

instance Read Suit where
  readPrec = do
    c <- get
    case c of
      'C' -> return Clubs
      'D' -> return Diamonds
      'H' -> return Hearts
      'S' -> return Spades
      _   -> mzero

instance Read Card where
  readPrec = do
    rk <- readPrec
    st <- readPrec
    return Card { rank = rk, suit = st }

instance Read Hand where
  readPrec = do
    c <- readPrec
    cs <- replicateM 4 (do ' ' <- get; readPrec)
    return $ hand (c:cs)

$(derive makeSerial ''Rank)
$(derive makeSerial ''Suit)
$(derive makeSerial ''Card)

instance Serial Hand where
  series d =  map hand $ tuples 5 [ Card { rank = rk, suit = st } |
                rk <- series 1, st <- take d (series 1) ]
    where tuples 0 cs     = [[]]
          tuples n []     = []
          tuples n (c:cs) = map (c:) (tuples (n-1) cs) ++ tuples n cs
  coseries = error "Serial.coseries: undefined for Poker.Hand"

prop_readShowCard :: Card -> Bool
prop_readShowCard c = c == read (show c)

prop_readShowHand :: Hand -> Bool
prop_readShowHand h = h == read (show h)
