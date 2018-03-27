
module IRGen where

import Control.Monad.State

import qualified AST
import qualified Check
import qualified Context
import qualified IndexExpr
import qualified IRGenContext
import qualified LoopIR
import qualified TensorExpr
import qualified TType
import qualified Utility


type Indices  = [String]
type IRGenCtx = IRGenContext.IRGenContext
type IRStmt   = LoopIR.Stmt String String Int
type TExpr    = TensorExpr.TExpr String String Int

exprType :: IRGenCtx -> AST.Expr -> TType.TType
exprType irgc e = let ctx   = IRGenContext.astCtx irgc
                      mtype = Check.checkExpr ctx e
                  in case mtype of
                     Just t  -> t
                     Nothing -> error "IR generator encountered ill-typed expression"

exprRank :: IRGenCtx -> AST.Expr -> Int
exprRank irgc e = TType.rank $ exprType irgc e

splitIndexList :: IRGenCtx -> AST.Expr -> AST.Expr -> [i] -> ([i], [i])
splitIndexList irgc e0 _ is = Utility.splitList (exprRank irgc e0) is

-- dispatch:
fromASTExpr :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExpr is e@(AST.Id _)       = fromASTExprId is e
fromASTExpr is e@(AST.Plus _ _)   = fromASTExprPlus is e
fromASTExpr is e@(AST.Minus _ _)  = fromASTExprMinus is e
fromASTExpr is e@(AST.Star _ _)   = fromASTExprStar is e
fromASTExpr is e@(AST.Slash _ _)  = error "not supported yet"
fromASTExpr is e@(AST.Hash _ _)   = fromASTExprHash is e
fromASTExpr is e@(AST.Period _ _) = fromASTExprPeriod is e
fromASTExpr is e@(AST.Caret _ _)  = fromASTExprCaret is e


fromASTExprId :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprId is (AST.Id n) = let iexprs = map IndexExpr.makeIndex is
                                  te     = TensorExpr.makeAccess n iexprs
                              in do return te

fromASTExprPlus :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprPlus is (AST.Plus e0 e1) = do te0 <- fromASTExpr is e0
                                         te1 <- fromASTExpr is e1
                                         return $ te0 TensorExpr.+ te1
                                        
fromASTExprMinus :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprMinus is (AST.Minus e0 e1) = do te0 <- fromASTExpr is e0
                                           te1 <- fromASTExpr is e1
                                           return $ te0 TensorExpr.- te1

fromASTExprStar :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprStar is (AST.Star e0 e1) = do irgc <- get
                                         let is0 = if exprRank irgc e0 == 0
                                                      then []
                                                      else is
                                         te0  <- fromASTExpr is0 e0
                                         te1  <- fromASTExpr is e1
                                         return $ te0 TensorExpr.* te1

fromASTExprHash :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprHash is (AST.Hash e0 e1) = do irgc <- get
                                         let (is0, is1) = splitIndexList irgc e0 e1 is
                                         te0  <- fromASTExpr is0 e0
                                         te1  <- fromASTExpr is1 e1
                                         return $  te0 TensorExpr.* te1

fromASTExprPeriod :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprPeriod is (AST.Period e p) = do i   <- IRGenContext.freshIndex
                                           let is' = Utility.insertPairIntoList p (i, i) is
                                           te' <- fromASTExpr is' e
                                           return $ TensorExpr.makeContraction i te'
                                           
fromASTExprCaret :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprCaret is (AST.Caret e p) = let is' = Utility.swapPair p is
                                      in fromASTExpr is' e

fromASTStmt :: AST.Stmt -> State IRGenCtx IRStmt
fromASTStmt stmt = let expr = AST.expr stmt
                       var  = AST.var stmt
                   in do irgc <- get
                         is   <- IRGenContext.freshIndices (exprRank irgc expr)
                         let iexprs = map IndexExpr.makeIndex is
                         let lhs    = TensorExpr.makeAccess var iexprs
                         rhs  <- fromASTExpr is expr
                         let irstmt = LoopIR.makeAssignment lhs rhs
                         return $ irLoopNest (exprType irgc expr) is irstmt
                         
fromASTStmts :: [AST.Stmt] -> State IRGenCtx IRStmt
fromASTStmts stmts = let folder = \irs -> \stmt -> do { ir <- fromASTStmt stmt;
                                                        return (ir:irs) }
                     in do irstmts <- foldM folder [] stmts
                           return $ LoopIR.makeCompound (reverse irstmts)
                           
fromASTProgram :: AST.Program -> IRStmt
fromASTProgram p = let decls = AST.decls p
                       stmts = AST.stmts p
                       ctx   = Context.fromDecls decls
                       irgc  = IRGenContext.makeIRGenContext ctx
                   in evalState (fromASTStmts stmts) irgc

irLoopNest :: TType.TType -> Indices -> IRStmt -> IRStmt
irLoopNest t is irstmt = let pairs  = TType.zip t is
                             folder = \(bound, index) ->
                                      \ir -> LoopIR.makeLoop index 0 bound 1 ir
                         in foldr folder irstmt pairs 
                                         
