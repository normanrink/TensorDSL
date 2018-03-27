
module LoopIR ( Stmt
              , makeAssignment
              , makeInitialization
              , makeDeclaration
              , makeCompound
              , makeLoop
              , makeElementCType
              , printCStmt ) where

import Text.PrettyPrint

import qualified TensorExpr
import qualified Utility as Util


-- n: name, i: index, c: constant
data Stmt n i c = Assignment { lhs :: TensorExpr.TExpr n i c
                             , rhs :: TensorExpr.TExpr n i c }
                | Initialization { lhs :: TensorExpr.TExpr n i c }
                | Declaration { name :: n
                              , dims :: [c] }    
                | Compound [Stmt n i c]
                | Loop { index :: i
                       , lb    :: c 
                       , ub    :: c
                       , inc   :: c
                       , body  :: Stmt n i c }
                deriving (Eq)


data ElementCType = ECT { typeString :: String
                        , initVal    :: String }
                  deriving (Eq, Show)

makeElementCType :: String -> String -> ElementCType
makeElementCType = ECT


makeAssignment :: TensorExpr.TExpr n i c -> TensorExpr.TExpr n i c -> Stmt n i c
makeAssignment lhs rhs = Assignment lhs rhs

makeInitialization :: TensorExpr.TExpr n i c -> Stmt n i c
makeInitialization lhs = Initialization lhs

makeDeclaration :: n -> [c] -> Stmt n i c
makeDeclaration name dims = Declaration name dims

makeCompound :: [Stmt n i c] -> Stmt n i c
makeCompound stmts = Compound stmts

makeLoop :: i -> c -> c -> c -> Stmt n i c -> Stmt n i c
makeLoop index lb ub inc body = Loop index lb ub inc body


printCStmt :: (Show n, Show i, Show c) => ElementCType -> Stmt n i c -> String
printCStmt eltty stmt = render $ prettyPrint eltty stmt

prettyPrint :: (Show n, Show i, Show c) => ElementCType -> Stmt n i c -> Doc
prettyPrint = prettyPrint' 0

prettyPrint' :: (Show n, Show i, Show c) => Int -> ElementCType -> Stmt n i c -> Doc
prettyPrint' indent _ (Assignment lhs rhs) =
  let lhs' = TensorExpr.prettyPrint lhs
      rhs' = TensorExpr.prettyPrint rhs
      eqop = (text "=")
  in nest indent $ lhs' <+> eqop <+> rhs' <> semi
prettyPrint' indent eltty (Initialization lhs) =
  let lhs' = TensorExpr.prettyPrint lhs
      eqop = (text $ "=")
      init = (text $ initVal eltty)
  in nest indent $ lhs' <+> eqop <+> init <> semi
prettyPrint' indent eltty (Declaration name dims) =
  let eltty' = (text $ typeString eltty)
      name'  = (text $ Util.showUnquoted name)
      dims'  = (text $ show dims)
  in nest indent $ eltty' <+> name' <> dims' <> semi
prettyPrint' indent eltty (Compound stmts) =
  let body    = vcat $ map (prettyPrint eltty) stmts
  in nest indent $ body
prettyPrint' indent eltty (Loop index lb ub inc body) =
  let body'   = prettyPrint eltty body
      eqop    = (text "=")
      leqop   = (text "<")
      incop   = (text "+=")
      for     = (text "for")
      int     = (text "int")
      index'  = (text $ Util.showUnquoted index)
  in nest indent $ for <+> lparen <> int <+> index' <+> eqop <+> (text $ show lb) <> semi
                       <+> index' <+> leqop <+> (text $ show ub) <> semi
                       <+> index' <+> incop <+> (text $ show inc) <> rparen
                       <+> lbrace $$  (nest 2 body') $$ rbrace
