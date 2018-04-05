
module IRGen ( fromASTProgram ) where

import Control.Monad.State

import qualified AST
import qualified Check
import qualified Context
import qualified IndexExpr
import qualified IRGenContext
import qualified LoopIR
import qualified Symbol
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

exprTypeAt :: IRGenCtx -> AST.Expr -> Int -> Int
exprTypeAt irgc e i = TType.at i (exprType irgc e)

splitIndexList :: IRGenCtx -> AST.Expr -> AST.Expr -> [i] -> ([i], [i])
splitIndexList irgc e0 _ is = Utility.splitList (exprRank irgc e0) is

-- dispatch:
fromASTExpr :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExpr is e@(AST.InE (AST.Id _))       = fromASTExprId is e
fromASTExpr is e@(AST.InE (AST.Plus _ _))   = fromASTExprPlus is e
fromASTExpr is e@(AST.InE (AST.Minus _ _))  = fromASTExprMinus is e
fromASTExpr is e@(AST.InE (AST.Star _ _))   = fromASTExprStar is e
fromASTExpr is e@(AST.InE (AST.Slash _ _))  = error "not supported yet"
fromASTExpr is e@(AST.InE (AST.Hash _ _))   = fromASTExprHash is e
fromASTExpr is e@(AST.InE (AST.Period _ _)) = fromASTExprPeriod is e
fromASTExpr is e@(AST.InE (AST.Caret _ _))  = fromASTExprCaret is e


fromASTExprId :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprId is (AST.InE (AST.Id n)) =
  let iexprs = map IndexExpr.makeIndex is
      te     = TensorExpr.makeAccess n iexprs
  in return te

fromASTExprPlus :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprPlus is (AST.InE (AST.Plus e0 e1)) =
  do te0 <- fromASTExpr is e0
     te1 <- fromASTExpr is e1
     return $ te0 TensorExpr.+ te1
                                        
fromASTExprMinus :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprMinus is (AST.InE (AST.Minus e0 e1)) =
  do te0 <- fromASTExpr is e0
     te1 <- fromASTExpr is e1
     return $ te0 TensorExpr.- te1

fromASTExprStar :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprStar is (AST.InE (AST.Star e0 e1)) =
  do irgc <- get
     let is0 = if exprRank irgc e0 == 0 then [] else is
     te0  <- fromASTExpr is0 e0
     te1  <- fromASTExpr is e1
     return $ te0 TensorExpr.* te1

fromASTExprHash :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprHash is (AST.InE (AST.Hash e0 e1)) =
  do irgc <- get
     let (is0, is1) = splitIndexList irgc e0 e1 is
     te0  <- fromASTExpr is0 e0
     te1  <- fromASTExpr is1 e1
     return $  te0 TensorExpr.* te1

fromASTExprPeriod :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprPeriod is (AST.InE (AST.Period e p@(p0, p1))) =
  do irgc <- get
     let ub  = exprTypeAt irgc e p0
     i    <- IRGenContext.freshIndex
     let is' = Utility.insertPairIntoList p (i, i) is
     te'  <- fromASTExpr is' e
     return $ TensorExpr.makeContraction i 0 ub 1 te'
                                           
fromASTExprCaret :: Indices -> AST.Expr -> State IRGenCtx TExpr
fromASTExprCaret is (AST.InE (AST.Caret e p)) =
  let is' = Utility.swapPair p is
  in fromASTExpr is' e

fromASTStmt :: AST.Stmt -> State IRGenCtx IRStmt
fromASTStmt stmt = let expr = AST.expr stmt
                       var  = AST.var stmt
                   in do irgc <- get
                         is   <- IRGenContext.freshIndices (exprRank irgc expr)
                         let iexprs = map IndexExpr.makeIndex is
                         let lhs    = TensorExpr.makeAccess var iexprs
                         rhs  <- fromASTExpr is expr
                         let eqop   = LoopIR.assignEq
                         let irstmt = LoopIR.makeAssignment eqop lhs rhs
                         return $ irLoopNest (exprType irgc expr) is irstmt
                         
fromASTStmts :: [AST.Stmt] -> State IRGenCtx IRStmt
fromASTStmts stmts = let folder = \irs -> \stmt -> do { ir <- fromASTStmt stmt;
                                                        return (ir:irs) }
                     in do irstmts <- foldM folder [] stmts
                           return $ LoopIR.makeCompound (reverse irstmts)
                           
fromASTProgram' :: AST.Program -> Context.Context -> (IRStmt, IRGenCtx)
fromASTProgram' p ctx = let decls = AST.decls p
                            stmts = AST.stmts p
                            irgc  = IRGenContext.makeIRGenContext ctx
                        in runState (fromASTStmts stmts) irgc

fromASTProgram :: AST.Program -> Context.Context -> IRStmt
fromASTProgram p ctx = let (ir, irgc) = fromASTProgram' p ctx
                           locals     = Context.locals ctx
                           irdecls    = irDeclarations locals
                           irstmt     = evalState (explicitContractions True ir) irgc
                       in LoopIR.makeCompound (irdecls ++ [irstmt])

irLoopNest :: TType.TType -> Indices -> IRStmt -> IRStmt
irLoopNest t is irstmt = let pairs  = TType.zip t is
                             folder = \(bound, index) ->
                                      \ir -> LoopIR.makeLoop index 0 bound 1 ir
                         in foldr folder irstmt pairs 

irDeclaration :: Symbol.Symbol -> IRStmt
irDeclaration sym = let name  = Symbol.symbolName sym
                        ttype = Symbol.symbolType sym
                    in LoopIR.makeDeclaration name ttype False

irDeclarations :: [Symbol.Symbol] -> [IRStmt]
irDeclarations syms = map irDeclaration syms

explicitContractionTopLevel :: IRStmt -> IRStmt
explicitContractionTopLevel ir =
  case LoopIR.isAssignment ir of
       False -> undefined
       True  -> let lhs   = LoopIR.lhs ir
                    rhs   = LoopIR.rhs ir
                    init  = LoopIR.makeInitialization lhs
                    addop = LoopIR.assignAdd
                in case TensorExpr.isContraction rhs of
                        False -> undefined
                        True  -> let index  = TensorExpr.index rhs
                                     lb     = TensorExpr.lb rhs
                                     ub     = TensorExpr.ub rhs
                                     inc    = TensorExpr.inc rhs
                                     e      = TensorExpr.expr rhs
                                     assign = LoopIR.makeAssignment addop lhs e
                                     loop   = LoopIR.makeLoop index lb ub inc assign 
                                 in LoopIR.makeCompound [init, loop]
                                    
explicitContractionFreshVar :: TExpr -> State IRGenCtx (TExpr, IRStmt, IRStmt)
explicitContractionFreshVar e =
  case TensorExpr.isContraction e of
       False -> undefined
       True  -> let i   = TensorExpr.index e
                    lb  = TensorExpr.lb e
                    ub  = TensorExpr.ub e
                    inc = TensorExpr.inc e
                    e'  = TensorExpr.expr e
                in do v <- IRGenContext.freshVariable
                      let decl    = LoopIR.makeDeclaration v TType.scalar True
                      let access  = TensorExpr.makeAccess v []
                      let addop   = LoopIR.assignAdd
                      let assign  = LoopIR.makeAssignment addop access e'
                      let loop    = LoopIR.makeLoop i lb ub inc assign
                      return (access, decl, loop)

explicitContractionsNested :: (TExpr, [IRStmt]) -> State IRGenCtx (TExpr, [IRStmt])
explicitContractionsNested (e, irs) | TensorExpr.isBinop e =
                                      let (e0, e1, maker) = TensorExpr.decomposeBinop e
                                      in do (e0', irs')  <- explicitContractionsNested (e0, irs)
                                            (e1', irs'') <- explicitContractionsNested (e1, irs')
                                            return (maker e0' e1', irs'')
                                    | TensorExpr.isContraction e =
                                      do (e', declir, loopir) <- explicitContractionFreshVar e
                                         let bodyir  = LoopIR.getBody loopir
                                         bodyir' <- explicitContractions False bodyir
                                         let loopir' = LoopIR.replaceBody loopir bodyir'
                                         return (e', irs ++ [declir, loopir'])
                                    | otherwise = return (e, irs)

explicitContractionsInAssignment :: Bool -> IRStmt -> State IRGenCtx IRStmt
explicitContractionsInAssignment top ir | LoopIR.isAssignment ir =
                                            let lhs = LoopIR.lhs ir
                                                rhs = LoopIR.rhs ir
                                            in if top && TensorExpr.isContraction rhs
                                               then let ir' = explicitContractionTopLevel ir
                                                    in explicitContractions False ir'
                                               else do (e, irs) <- explicitContractionsNested (rhs, [])
                                                       let eqop   = LoopIR.assignOp ir
                                                       let assign = LoopIR.makeAssignment eqop lhs e
                                                       let stmt   = LoopIR.makeCompound (irs ++ [assign])
                                                       return stmt
                                        | otherwise = undefined

explicitContractions :: Bool -> IRStmt -> State IRGenCtx IRStmt
explicitContractions top ir | LoopIR.isAssignment ir =
                                explicitContractionsInAssignment top ir
                            | LoopIR.isCompound ir =
                                let folder = \irs ->
                                             \stmt -> do { ir' <- explicitContractions top stmt;
                                                           return (ir':irs) }
                                in do irs' <- foldM folder [] (LoopIR.getStmts ir)
                                      return $ LoopIR.makeCompound (reverse irs')
                            | LoopIR.isLoop ir =
                                let body  = LoopIR.body ir
                                in do body' <- explicitContractions top body
                                      return $ LoopIR.replaceBody ir body'
                            | LoopIR.isDeclaration ir =
                                return ir
                            | LoopIR.isInitialization ir =
                                return ir
