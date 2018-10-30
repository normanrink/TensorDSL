
module TensorExpr ( TExpr(..)
                  , makeAccess
                  , makeContraction, isContraction, replaceContractionExpr
                  , (+)
                  , (-)
                  , (*)
                  , isBinop, decomposeBinop
                  , show
                  , tree
                  , prettyPrint
                  , freeIndicesInExpr ) where

import Prelude hiding ((+), (-), (*), (<>))
import qualified Prelude ((+), (-), (*))
import Text.PrettyPrint

import qualified IndexExpr
import qualified Utility as Util


-- n: name, i: index, c: constant
data TExpr n i c = Add (TExpr n i c) (TExpr n i c)
                 | Sub (TExpr n i c) (TExpr n i c)
                 | Mul (TExpr n i c) (TExpr n i c)
                 | Contr { index :: i
                         , lb    :: c
                         , ub    :: c
                         , inc   :: c
                         , expr  :: TExpr n i c }
                 | Acc n [IndexExpr.IExpr i c]
                 deriving (Eq)

infixl 6 +
(+) :: TExpr n i c -> TExpr n i c -> TExpr n i c
(+) e0 e1 = Add e0 e1

infixl 6 -
(-) :: TExpr n i c -> TExpr n i c -> TExpr n i c
(-) e0 e1 = Sub e0 e1

infixl 7 *
(*) :: TExpr n i c -> TExpr n i c -> TExpr n i c
(*) e0 e1 = Mul e0 e1

makeAccess :: n -> [IndexExpr.IExpr i c] -> TExpr n i c
makeAccess name is = Acc name is

makeContraction :: i -> c -> c -> c -> TExpr n i c -> TExpr n i c
makeContraction index lb ub inc e = Contr index lb ub inc e

isContraction :: TExpr n i c -> Bool
isContraction (Contr _ _ _ _ _) = True
isContraction _                 = False

replaceContractionExpr :: TExpr n i c -> TExpr n i c -> TExpr n i c
replaceContractionExpr (Contr index lb ub inc expr) expr' = Contr index lb ub inc expr'
replaceContractionExpr _                            _     = undefined

isBinop :: TExpr n i c -> Bool
isBinop (Add _ _) = True
isBinop (Sub _ _) = True
isBinop (Mul _ _) = True
isBinop _         = False

decomposeBinop :: TExpr n i c -> (TExpr n i c, TExpr n i c,
                                  TExpr n i c -> TExpr n i c -> TExpr n i c)
decomposeBinop (Add e0 e1) = (e0, e1, (+))
decomposeBinop (Sub e0 e1) = (e0, e1, (-))
decomposeBinop (Mul e0 e1) = (e0, e1, (*))
decomposeBinop _           = undefined


instance (Show n, Show i, Show c) => Show (TExpr n i c) where
  show = render . prettyPrint
  
prettyPrint :: (Show n, Show i, Show c) => TExpr n i c -> Doc
prettyPrint e@(Add e0 e1) = let op = (text $ exprKindAsOp e)
                            in parens $ (prettyPrint e0) <> op <> (prettyPrint e1)
prettyPrint e@(Sub e0 e1) = let op = (text $ exprKindAsOp e)
                            in parens $ (prettyPrint e0) <> op <> (prettyPrint e1)
prettyPrint e@(Mul e0 e1) = let op = (text $ exprKindAsOp e)
                            in (prettyPrint e0) <> op <> (prettyPrint e1)
prettyPrint (Contr index lb ub inc e) =
  let label = (text "Contract")
      args  = [ (text $ Util.showUnquoted index)
              , (text $ show lb)
              , (text $ show ub)
              , (text $ show inc)
              , (prettyPrint e)]
  in label <> (parens . hsep $ punctuate comma args)
prettyPrint (Acc name is) = (text $ Util.showUnquoted name) <> (prettyIndices is)

prettyIndices :: (Show i, Show c) => [IndexExpr.IExpr i c] -> Doc
prettyIndices is = if length is == 0
                      then empty
                      else hcat $ map (brackets . IndexExpr.prettyPrintTopLevel) is

tree :: (Show n, Show i, Show c) => TExpr n i c -> String
tree = render . formatAsTree

formatAsTree :: (Show n, Show i, Show c) => TExpr n i c -> Doc
formatAsTree = formatAsTree' 0

formatAsTree' :: (Show n, Show i, Show c) => Int -> TExpr n i c -> Doc
formatAsTree' indent e@(Add e0 e1)   = let kind = exprKindAsString e
                                       in formatBinOp indent kind e0 e1
formatAsTree' indent e@(Sub e0 e1)   = let kind = exprKindAsString e
                                       in formatBinOp indent kind e0 e1
formatAsTree' indent e@(Mul e0 e1)   = let kind = exprKindAsString e
                                       in formatBinOp indent kind e0 e1
formatAsTree' indent e@(Acc name is) = let kind  = exprKindAsString e
                                           label = Util.showUnquoted name
                                       in nest indent $ (text label) <> prettyIndices is

formatBinOp :: (Show n, Show i, Show c) => Int -> String -> TExpr n i c -> TExpr n i c -> Doc
formatBinOp indent kind e0 e1 = let indent' = (length kind) Prelude.+ 1
                                in nest indent $ (text kind) $+$
                                   (formatAsTree' indent' e0) $+$
                                   (formatAsTree' indent' e1)


exprKindAsOp :: TExpr n i c -> String
exprKindAsOp (Add _ _) = "+"
exprKindAsOp (Sub _ _) = "-"
exprKindAsOp (Mul _ _) = "*"
exprKindAsOp _         = error "not an operator"

exprKindAsString :: TExpr n i c -> String
exprKindAsString (Add _ _)         = "Add"
exprKindAsString (Sub _ _)         = "Sub"
exprKindAsString (Mul _ _)         = "Mul"
exprKindAsString (Contr _ _ _ _ _) = "Contr"
exprKindAsString (Acc _ _)         = "Acc"


freeIndicesInExpr :: (Eq i) => TExpr n i c -> [i]
freeIndicesInExpr (Add e0 e1)        = (freeIndicesInExpr e0) ++ (freeIndicesInExpr e1)
freeIndicesInExpr (Sub e0 e1)        = (freeIndicesInExpr e0) ++ (freeIndicesInExpr e1)
freeIndicesInExpr (Mul e0 e1)        = (freeIndicesInExpr e0) ++ (freeIndicesInExpr e1)
freeIndicesInExpr (Contr i _ _ _ e0) = [ j | j <- (freeIndicesInExpr e0), j /= i ]
freeIndicesInExpr (Acc _ is)         = concatMap IndexExpr.indicesInExpr is
