
module LoopIR ( AssignOp, assignEq, assignAdd
              , Stmt, assignOp, lhs, rhs, index, lb, ub, inc, body
              , makeAssignment, isAssignment
              , makeInitialization, isInitialization
              , makeDeclaration, isDeclaration
              , makeCompound, isCompound, getStmts 
              , makeLoop, isLoop, getBody, replaceBody
              , makeElementCType
              , printCStmt', printCStmt ) where

import Text.PrettyPrint

import qualified TensorExpr
import qualified Utility as Util


data AssignOp = AssignEq | AssignAdd
              deriving (Eq)

assignEq :: AssignOp
assignEq = AssignEq

assignAdd :: AssignOp
assignAdd = AssignAdd

instance Show AssignOp where
  show AssignEq  = "="
  show AssignAdd = "+="
  
-- n: name, i: index, c: constant
data Stmt n i c = Assignment { op  :: AssignOp
                             , lhs :: TensorExpr.TExpr n i c
                             , rhs :: TensorExpr.TExpr n i c }
                | Initialization { lhs :: TensorExpr.TExpr n i c }
                | Declaration { name :: n
                              , dims :: [c]
                              , init :: Bool }    
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


assignOp :: Stmt n i c -> AssignOp
assignOp (Assignment op _ _) = op
assignOp _                   = undefined

makeAssignment :: AssignOp -> TensorExpr.TExpr n i c -> TensorExpr.TExpr n i c -> Stmt n i c
makeAssignment op lhs rhs = Assignment op lhs rhs

isAssignment :: Stmt n i c -> Bool
isAssignment (Assignment _ _ _) = True
isAssignment _                  = False

makeInitialization :: TensorExpr.TExpr n i c -> Stmt n i c
makeInitialization lhs = Initialization lhs

isInitialization :: Stmt n i c -> Bool
isInitialization (Initialization _) = True
isInitialization _                  = False

makeDeclaration :: n -> [c] -> Bool -> Stmt n i c
makeDeclaration name dims init = Declaration name dims init

isDeclaration :: Stmt n i c -> Bool
isDeclaration (Declaration _ _ _) = True
isDeclaration _                   = False

makeCompound :: [Stmt n i c] -> Stmt n i c
makeCompound stmts = Compound stmts

isCompound :: Stmt n i c -> Bool
isCompound (Compound _) = True
isCompound _            = False

getStmts :: Stmt n i c -> [Stmt n i c]
getStmts (Compound stmts) = stmts
getStmts _                = undefined

makeLoop :: i -> c -> c -> c -> Stmt n i c -> Stmt n i c
makeLoop index lb ub inc body = Loop index lb ub inc body

isLoop :: Stmt n i c -> Bool
isLoop (Loop _ _ _ _ _) = True
isLoop _                = False

getBody :: Stmt n i c -> Stmt n i c
getBody s@(Loop _ _ _ _ _) = body s
getBody _                  = undefined

replaceBody :: Stmt n i c -> Stmt n i c -> Stmt n i c
replaceBody (Loop index ub lb inc body) body' = Loop index ub lb inc body'
replaceBody _                       _     = undefined


printCStmt :: (Show n, Show i, Show c) => ElementCType -> Stmt n i c -> String
printCStmt eltty stmt = render $ printCStmt' eltty stmt

printCStmt' :: (Show n, Show i, Show c) => ElementCType -> Stmt n i c -> Doc
printCStmt' = prettyPrint' 0

prettyPrint' :: (Show n, Show i, Show c) => Int -> ElementCType -> Stmt n i c -> Doc
prettyPrint' indent _ (Assignment op lhs rhs) =
  let lhs' = TensorExpr.prettyPrint lhs
      rhs' = TensorExpr.prettyPrint rhs
      eqop = (text $ show op)
  in nest indent $ lhs' <+> eqop <+> rhs' <> semi
prettyPrint' indent eltty (Initialization lhs) =
  let lhs' = TensorExpr.prettyPrint lhs
      eqop = (text $ "=")
      init = (text $ initVal eltty)
  in nest indent $ lhs' <+> eqop <+> init <> semi
prettyPrint' indent eltty (Declaration name dims init) =
  let name'  = (text $ Util.showUnquoted name)
      dims'  = if length dims == 0
                  then empty
                  else hcat $ map (brackets . text . show) dims
      eltty' = (text $ typeString eltty)
      decl   = eltty' <+> name' <> dims'
      init'  = if init
                  -- TODO: intialization like this only works for scalars.
                  then (text "=") <+> (text $ initVal eltty)
                  else empty
  in nest indent $ decl <+> init' <> semi
prettyPrint' indent eltty (Compound stmts) =
  let body    = vcat $ map (printCStmt' eltty) stmts
  in nest indent $ body
prettyPrint' indent eltty (Loop index lb ub inc body) =
  let body'   = printCStmt' eltty body
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
