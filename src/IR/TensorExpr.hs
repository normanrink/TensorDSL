
module TensorExpr ( TExpr
                  , makeAccess
                  , makeContraction
                  , (+)
                  , (-)
                  , (*)
                  , show
                  , tree
                  , prettyPrint
                  , indicesInExpr ) where

import Prelude hiding ((+), (-), (*))
import qualified Prelude ((+), (-), (*))
import Text.PrettyPrint

import qualified IndexExpr
import qualified Utility as Util


-- n: name, i: index, c: constant
data TExpr n i c = Add (TExpr n i c) (TExpr n i c)
                 | Sub (TExpr n i c) (TExpr n i c)
                 | Mul (TExpr n i c) (TExpr n i c)
                 | Contr i (TExpr n i c)
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

makeContraction :: i -> TExpr n i c -> TExpr n i c
makeContraction index e = Contr index e


instance (Show n, Show i, Show c) => Show (TExpr n i c) where
  show = render . prettyPrint
  
prettyPrint :: (Show n, Show i, Show c) => TExpr n i c -> Doc
prettyPrint e@(Add e0 e1) = let op = (text $ exprKindAsOp e)
                            in parens $ (prettyPrint e0) <> op <> (prettyPrint e1)
prettyPrint e@(Sub e0 e1) = let op = (text $ exprKindAsOp e)
                            in parens $ (prettyPrint e0) <> op <> (prettyPrint e1)
prettyPrint e@(Mul e0 e1) = let op = (text $ exprKindAsOp e)
                            in (prettyPrint e0) <> op <> (prettyPrint e1)
prettyPrint (Contr _ e)   = (prettyPrint e) -- leave contractions implicit
prettyPrint (Acc name is) = (text $ Util.showUnquoted name) <> (prettyIndices is)

prettyIndices :: (Show i, Show c) => [IndexExpr.IExpr i c] -> Doc
prettyIndices is = hcat $ map (brackets . IndexExpr.prettyPrintTopLevel) is

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
exprKindAsString (Add _ _)   = "Add"
exprKindAsString (Sub _ _)   = "Sub"
exprKindAsString (Mul _ _)   = "Mul"
exprKindAsString (Contr _ _) = "Contr"
exprKindAsString (Acc _ _)   = "Acc"


indicesInExpr :: (Eq i) => TExpr n i c -> [i]
indicesInExpr (Add e0 e1)  = (indicesInExpr e0) ++ (indicesInExpr e1)
indicesInExpr (Sub e0 e1)  = (indicesInExpr e0) ++ (indicesInExpr e1)
indicesInExpr (Mul e0 e1)  = (indicesInExpr e0) ++ (indicesInExpr e1)
indicesInExpr (Contr i e0) = [ j | j <- (indicesInExpr e0), j /= i ]
indicesInExpr (Acc _ is)   = concatMap IndexExpr.indicesInExpr is
