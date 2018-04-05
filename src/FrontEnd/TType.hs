
module TType ( TType
             , rank, at
             , concat
             , scalar
             , isScalar
             , skipPair, swapPair
             , zip
             , prettyTType, prettyCTType ) where

import Prelude hiding (concat, zip)
import qualified Prelude (zip)
import Text.PrettyPrint

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

prettyTType :: TType -> Doc
prettyTType t = hcat $ map (brackets . text . show) t

prettyCTType :: TType -> Bool -> Doc
-- TODO: The 'empty' type for scalars is not ideal for
-- formal function arguments in C (since this way scalar
-- results can not be passed out of the kernel).
prettyCTType t isConst | isScalar t = empty
                       | otherwise =
                           let restrict = text "restrict"
                               const    = text (if isConst then "const" else "")
                               first = (text . show . head) t
                               rest  = hcat $ map (brackets . text . show) (tail t)
                           in (brackets $ restrict <+> const <+> first) <> rest
