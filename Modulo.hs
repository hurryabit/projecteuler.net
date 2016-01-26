{-# LANGUAGE TemplateHaskell #-}
module Modulo where

import Language.Haskell.TH

createZ n = do
    let typ = mkName $ "Z" ++ show n
        con = typ
        sel = mkName $ "z" ++ show n ++ "toInt"
--    return
--        [ NewtypeD [] typ [] (RecC con [(sel,NotStrict,ConT (mkName "Int"))]) (map mkName ["Eq","Ord"])
--        ]
    [d| type $(typ) = () |]
