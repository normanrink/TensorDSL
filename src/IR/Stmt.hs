
module Stmt ( makeAssignment
            , makeCompound
            , makeLoop )where

import Text.PrettyPrint

import qualified TensorExpr
import qualified Utility as Util


-- n: name, i: index, c: constant
data Stmt n i c = Assignment { lhs :: TensorExpr.TExpr n i c
                             , rhs :: TensorExpr.TExpr n i c }
                | Compound [Stmt n i c]
                | Loop { index :: i
                       , lb    :: c 
                       , ub    :: c
                       , inc   :: c
                       , body  :: Stmt n i c }
                deriving (Eq)


makeAssignment :: TensorExpr.TExpr n i c -> TensorExpr.TExpr n i c -> Stmt n i c
makeAssignment lhs rhs = Assignment lhs rhs

makeCompound :: [Stmt n i c] -> Stmt n i c
makeCompound stmts = Compound stmts

makeLoop :: i -> c -> c -> c -> Stmt n i c -> Stmt n i c
makeLoop index lb ub inc body = Loop index lb ub inc body


instance (Show n, Show i, Show c) => Show (Stmt n i c) where
  show = render . prettyPrint

prettyPrint :: (Show n, Show i, Show c) => Stmt n i c -> Doc
prettyPrint = prettyPrint' 0

prettyPrint' :: (Show n, Show i, Show c) => Int -> Stmt n i c -> Doc
prettyPrint' indent (Assignment lhs rhs) =
  let lhs' = TensorExpr.prettyPrint lhs
      rhs' = TensorExpr.prettyPrint rhs
      eqop = (text "=")
  in nest indent $ lhs' <+> eqop <+> rhs' <> semi
prettyPrint' indent (Compound stmts) =
  let body    = vcat $ map prettyPrint stmts
  in nest indent $ body
prettyPrint' indent (Loop index lb ub inc body) =
  let body'   = prettyPrint body
      eqop    = (text "=")
      leqop   = (text "<=")
      incop   = (text "+=")
      for     = (text "for")
      int     = (text "int")
      index'  = (text $ Util.showUnquoted index)
  in nest indent $ for <+> lparen <> int <+> index' <+> eqop <+> (text $ show lb) <> semi
                       <+> index' <+> leqop <+> (text $ show ub) <> semi
                       <+> index' <+> incop <+> (text $ show inc) <> rparen
                       <+> lbrace $$  (nest 2 body') $$ rbrace
