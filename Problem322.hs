import Data.List

pows = iterate (*2) 4
rems = map ((10^12-10) `mod`) pows

clss = zipWith3 (\p r -> foldl combine [p-r .. p-1] . zip pows) pows rems (inits clss)
  where
    combine xs (p,ys) = filter (\x -> (x `mod` p) `notElem` ys) xs
