
module TType ( TType
             , rank
             , at
             , concat
             , scalar
             , isScalar
             , skipPair
             , swapPair
             , zip ) where

import Prelude hiding (concat, zip)
import qualified Prelude (zip)

import qualified AST
import qualified Utility


type TType = [Int]

rank :: TType -> Int
rank t = length t

at :: Int -> TType -> Int
at i t = t !! i

concat :: TType -> TType -> TType
concat t0 t1 = t0 ++ t1

scalar :: TType
scalar = []

isScalar :: TType -> Bool
isScalar [] = True
isScalar _  = False

skipPair :: AST.Pair -> TType -> TType
skipPair = Utility.skipPair

swapPair :: AST.Pair -> TType -> TType
swapPair = Utility.swapPair

zip :: TType -> [a] -> [(Int, a)]
zip = Prelude.zip
