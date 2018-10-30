
module IndexExpr ( IExpr
                 , makeIndex
                 , makeConst
                 , (+)
                 , (-)
                 , (*)
                 , isConstant
                 , show
                 , prettyPrint
                 , prettyPrintTopLevel
                 , tree
                 , formatAsTree
                 , indicesInExpr ) where

import Prelude hiding ((+), (-), (*), (<>))
import qualified Prelude ((+), (-), (*))
import Text.PrettyPrint

import qualified Utility as Util 


-- i: index, c: constant
data IExpr i c = Add (IExpr i c) (IExpr i c)
               | Sub (IExpr i c) (IExpr i c)
               | Mul (IExpr i c) (IExpr i c)
               | Index i
               | Const c
               deriving (Eq)

infixl 6 +
(+) :: IExpr i c -> IExpr i c -> IExpr i c
(+) e0 e1 = Add e0 e1

infixl 6 -
(-) :: IExpr i c -> IExpr i c -> IExpr i c
(-) e0 e1 = Sub e0 e1

infixl 7 *
(*) :: IExpr i c -> IExpr i c -> IExpr i c
(*) e0 e1 = Mul e0 e1

makeIndex :: i -> IExpr i c
makeIndex label = Index label

makeConst :: c -> IExpr i c
makeConst value = Const value


isConstant :: IExpr i c -> Bool
isConstant (Index _)   = False
isConstant (Const _)   = True
isConstant (Add e0 e1) = (isConstant e0) && (isConstant e1)
isConstant (Sub e0 e1) = (isConstant e0) && (isConstant e1)
isConstant (Mul e0 e1) = (isConstant e0) && (isConstant e1)


instance (Show i, Show c) => Show (IExpr i c) where
  show = render . prettyPrintTopLevel

prettyPrintTopLevel :: (Show i, Show c) => IExpr i c -> Doc
prettyPrintTopLevel e@(Add e0 e1) = let op = (text $ exprKindAsOp e)
                                    in (prettyPrint e0) <> op <> (prettyPrint e1)
prettyPrintTopLevel e@(Sub e0 e1) = let op = (text $ exprKindAsOp e)
                                    in (prettyPrint e0) <> op <> (prettyPrint e1)
prettyPrintTopLevel e@(Mul e0 e1) = let op = (text $ exprKindAsOp e)
                                    in (prettyPrint e0) <> op <> (prettyPrint e1) 
prettyPrintTopLevel (Index ind)   = (text $ Util.showUnquoted ind)
prettyPrintTopLevel (Const val)   = (text $ show val)

prettyPrint :: (Show i, Show c) => IExpr i c -> Doc
prettyPrint e@(Add _ _) = parens $ prettyPrintTopLevel e
prettyPrint e@(Sub _ _) = parens $ prettyPrintTopLevel e
prettyPrint e           = prettyPrintTopLevel e

tree :: (Show i, Show c) => IExpr i c -> String
tree = render . formatAsTree

formatAsTree :: (Show i, Show c) => IExpr i c -> Doc
formatAsTree = formatAsTree' 0

formatAsTree' :: (Show i, Show c) => Int -> IExpr i c -> Doc
formatAsTree' indent e@(Add e0 e1) = let kind = exprKindAsString e
                                     in formatBinOp indent kind e0 e1
formatAsTree' indent e@(Sub e0 e1) = let kind = exprKindAsString e
                                     in formatBinOp indent kind e0 e1
formatAsTree' indent e@(Mul e0 e1) = let kind = exprKindAsString e
                                     in formatBinOp indent kind e0 e1
formatAsTree' indent e@(Index ind) = let kind  = exprKindAsString e
                                         label = Util.showUnquoted ind
                                     in formatUnary indent kind label
formatAsTree' indent e@(Const val) = let kind  = exprKindAsString e
                                         label = show val
                                     in formatUnary indent kind label
                                            
formatBinOp :: (Show i, Show c) => Int -> String -> IExpr i c -> IExpr i c -> Doc
formatBinOp indent kind e0 e1 = let indent' = (length kind) Prelude.+ 1
                                in nest indent $ (text kind) $+$
                                   (formatAsTree' indent' e0) $+$
                                   (formatAsTree' indent' e1)

formatUnary :: Int -> String -> String -> Doc
formatUnary indent kind label = nest indent $ (text kind) <+> (text label)

exprKindAsOp :: IExpr i c -> String
exprKindAsOp (Add _ _) = "+"
exprKindAsOp (Sub _ _) = "-"
exprKindAsOp (Mul _ _) = "*"
exprKindAsOp _         = error "not an operator"

exprKindAsString :: IExpr i c -> String
exprKindAsString (Add _ _) = "Add"
exprKindAsString (Sub _ _) = "Sub"
exprKindAsString (Mul _ _) = "Mul"
exprKindAsString (Index _) = "Index"
exprKindAsString (Const _) = "Const"


indicesInExpr :: IExpr i c -> [i]
indicesInExpr (Add e0 e1) = (indicesInExpr e0) ++ (indicesInExpr e1)
indicesInExpr (Sub e0 e1) = (indicesInExpr e0) ++ (indicesInExpr e1)
indicesInExpr (Mul e0 e1) = (indicesInExpr e0) ++ (indicesInExpr e1)
indicesInExpr (Index i)   = [i]
indicesInExpr (Const _)   = []
