
module TType ( TType
             , concat
             , isScalar
             , skipPair
             , swapPair ) where

import Prelude hiding (concat)

import qualified AST


type TType = [Int]

concat :: TType -> TType -> TType
concat t0 t1 = t0 ++ t1

isScalar :: TType -> Bool
isScalar [] = True
isScalar _  = False

skip :: Int -> TType -> TType
skip i ds = take i ds ++ drop (i+1) ds

skipPair :: AST.Pair -> TType -> TType
skipPair (x0, x1) ds = let x0' = min x0 x1
                           x1' = max x0 x1
                       in skip (x1'-1) $ skip x0' ds

swapPair :: AST.Pair -> TType -> TType
swapPair (x0, x1) ds | x0 == x1 = ds
                     | otherwise =
                         let x0'    = min x0 x1
                             x1'    = max x0 x1
                             d0     = ds !! x0'
                             d1     = ds !! x1'
                             head   = take x0' ds
                             tail   = drop (x0'+1) ds
                             middle = take (x1'-x0'-1) tail
                             end    = drop (x1'-x0') tail
                         in head ++ [d1] ++ middle ++ [d0] ++ end
