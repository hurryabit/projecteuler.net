module Sudoku.Sudoku
  where

import Control.Monad (foldM, guard, mzero)
import Data.Array.IArray
import Data.Char (chr, ord)
import Data.Maybe (fromJust, mapMaybe)
import Data.List (minimumBy, intersperse)
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Ord (comparing)

import Sudoku.Grouping
import Sudoku.Util

data Field = Known Int | Unknown IntSet

data Board = Board
  { dimension :: Int
  , fields    :: Array Index Field
  , grouping  :: Grouping
  }

instance Show Board where
  show b = let fs = fields b
    in unlines $ multiInsert 3 "+-------+-------+-------+"
        [ intersperse ' ' $
            multiInsert 3 '|' [ showField (fs ! (i,j)) | j <- [0..8] ]
        | i <- [0..8]
        ]
    where
      showField :: Field -> Char
      showField (Known n) = chr (ord '0' + n)
      showField (Unknown s) = case Set.size s of
        1 -> '*'
        2 -> 'x'
        _ -> ' '


rowCount :: Board -> Int
rowCount = (1+) . fst . snd . bounds . fields

colCount :: Board -> Int
colCount = (1+) . snd . snd . bounds . fields

field :: Board -> Index -> Field
field b = ((fields b)!)

knownField :: Int -> Field
knownField = Known

isKnownField :: Field -> Bool
isKnownField (Known _) = True
isKnownField _         = False

emptyField :: Int -> Field
emptyField = Unknown . Set.fromDistinctAscList . enumFromTo 1

remainingNumbers :: Board -> Index  -> IntSet
remainingNumbers b i = case field b i of
  Unknown s -> s
  _         -> Set.empty

firstUnknownField :: Board -> Maybe (Index,[Int])
firstUnknownField b
  | null us   = Nothing
  | otherwise = Just (i,Set.toList s)
  where us    = [ (i',s') | (i',Unknown s') <- assocs (fields b) ]
        (i,s) = minimumBy (comparing (Set.size . snd)) us

emptyBoard :: Board
emptyBoard = Board
  { dimension = 9
  , fields    = listArray ((0,0),(8,8)) (repeat (emptyField 9))
  , grouping  = stdGrouping 3
  }

board :: [(Index,Int)] -> Board
board = fromJust . addNumbers emptyBoard

parseBoard :: [String] -> Board
parseBoard zs = board $ do
  (i,cs) <- zip [0..] zs
  (j,c)  <- zip [0..] cs
  guard ('1' <= c && c <= '9')
  return ((i,j),ord c-ord '0')

removeNumber :: Field -> Int -> Maybe Field
removeNumber f n = case f of
  Known m   -> guard (m /= n) >> return f
  Unknown s -> do
    let s' = n `Set.delete` s
    guard (not (Set.null s'))
    return (Unknown s')

removeOptions :: Board -> [(Index,Int)] -> Maybe Board
removeOptions b os = do
  fs <- accumM removeNumber (fields b) os
  return b { fields = fs }

addNumbers :: Board -> [(Index,Int)] -> Maybe Board
addNumbers = foldM (uncurry . addNumber) 

addNumber :: Board -> Index -> Int -> Maybe Board
addNumber b i n = do
  let fs = fields b
      rel = relation (grouping b)
  fs' <- accumM removeNumber fs $ zip (filter (rel i) (indices fs)) (repeat n)
  return b { fields = fs' // [(i,knownField n)] }

isUniqueUnknownField :: Field -> Bool
isUniqueUnknownField (Known _) = False
isUniqueUnknownField (Unknown s) = Set.size s == 1

uniqueUnknownFields :: Board -> [(Index,Int)]
uniqueUnknownFields b =
  [ (i, head (Set.elems s))
  | (i, Unknown s) <- filter (isUniqueUnknownField . snd) $ assocs $ fields b
  ]

uniqueGroupingFields :: Board -> [(Index,Int)]
uniqueGroupingFields b = concatMap (uniqueGroupFields b) (groups (grouping b))

uniqueGroupFields :: Board -> Group -> [(Index,Int)]
uniqueGroupFields b g = do 
  n <- [1..dimension b]
  case remainingGroupFields b g n of
    [i] -> return (i,n)
    _   -> mzero

remainingGroupFields :: Board -> Group -> Int -> [Index]
remainingGroupFields b g n = filter (Set.member n . remainingNumbers b) g

solve :: Board -> [Board]
solve b
  | null ks   = case firstUnknownField b of
                  Just (i,s)  -> concatMap solve $ mapMaybe (addNumber b i) s
                  Nothing     -> [b]
  | otherwise = maybe [] solve (addNumbers b ks)
  where ks  = uniqueUnknownFields b ++ uniqueGroupingFields b

countKnown :: Board -> Int
countKnown = length . filter isKnownField . elems . fields
