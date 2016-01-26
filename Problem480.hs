module Problem480 where

import Control.Arrow
import Data.Array 
import Data.List

text = "thereisasyetinsufficientdataforameaningfulanswer"

histgogram = accumArray (+) 0 ('a','z') (map (id &&& const 1) text)