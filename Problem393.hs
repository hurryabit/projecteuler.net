module Problem393 where

import Data.MemoCombinators

data Connector = None | In | Out
    deriving (Show,Eq,Ord,Enum)

type Config = [Connector]

advance :: Config -> [Config]
advance = advanceEx None

advanceEx :: Connector -> Config -> [Config]
advanceEx = memo2 enum (list enum) advanceEx'

advanceEx' :: Connector -> Config -> [Config]
advanceEx' ct []      = case ct of
    None -> [[]]
    _    -> []
advanceEx' ct (cl:cs) =
    let (c1,c2) = case ct `compare` cl of
            GT -> (cl,ct)
            _  -> (ct,cl)
    in  concatMap (\(c3,c4) -> map (c3:) (advanceEx c4 cs)) $ case (c1,c2) of
            (None,None) -> [(In  ,Out ),(Out ,In  )]
            (None,In  ) -> [(None,In  ),(In  ,None)]
            (None,Out ) -> [(None,Out ),(Out ,None)]
            (In  ,In  ) -> []
            (In  ,Out ) -> [(None,None)]
            (Out ,Out ) -> []

count :: Int -> Config -> Int
count = memo2 bits (list enum) count'

count' :: Int -> Config -> Int
count' n cs = case n of
    0 | all (None ==) cs -> 1
      | otherwise        -> 0
    _                    -> sum $ map (count (n-1)) (advance cs)

main = print $ count dim (replicate dim None)
    where dim = 8
