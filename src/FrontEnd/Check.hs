
module Check ( checkProgram
             , checkStmt
             , checkStmts
             , checkExpr ) where

import qualified AST
import qualified Context
import qualified Symbol
import qualified TType


skipPairF :: (Functor f) => AST.Pair -> f TType.TType -> f TType.TType
skipPairF p = fmap (TType.skipPair p)

swapPairF :: (Functor f) => AST.Pair -> f TType.TType -> f TType.TType
swapPairF p = fmap (TType.swapPair p)


type MType = Maybe TType.TType

isScalar :: MType -> Bool
isScalar (Just t) = TType.isScalar t
isScalar _        = False

checkProgram :: AST.Program -> (Bool, Context.Context)
checkProgram p = let decls = AST.decls p
                     stmts = AST.stmts p
                     ctx   = Context.fromDecls decls
                 in (checkStmts ctx stmts, ctx)
                                             
checkStmts :: Context.Context -> [AST.Stmt] -> Bool
checkStmts ctx stmts = and $ map (checkStmt ctx) stmts

checkStmt :: Context.Context -> AST.Stmt -> Bool
checkStmt ctx stmt = let tv = checkVar ctx (AST.var stmt)
                         te = checkExpr ctx (AST.expr stmt)
                      in tv == te
                         
getType :: Context.Context -> String -> TType.TType
getType ctx name = Symbol.symbolType $ Context.getSymbol name ctx

checkVar :: Context.Context -> String -> MType
checkVar ctx name | Context.inScope name ctx = Just $ getType ctx name
                  | otherwise                = Nothing

combine :: MType -> MType -> MType
combine t0 t1 | t0 == t1  = t0
              | otherwise = Nothing

concat :: MType -> MType -> MType
concat (Just t0) (Just t1) = Just $ TType.concat t0 t1
concat _         _         = Nothing

checkExpr :: Context.Context -> AST.Expr -> MType
checkExpr ctx (AST.Id name)     = checkVar ctx name
checkExpr ctx (AST.Plus e0 e1)  = let t0 = checkExpr ctx e0
                                      t1 = checkExpr ctx e1
                                  in combine t0 t1
checkExpr ctx (AST.Minus e0 e1) = let t0 = checkExpr ctx e0
                                      t1 = checkExpr ctx e1
                                  in combine t0 t1
checkExpr ctx (AST.Star e0 e1)  = let t0 = checkExpr ctx e0
                                      t1 = checkExpr ctx e1
                                  in if isScalar t0
                                        then t1
                                        else combine t0 t1
checkExpr ctx (AST.Slash e0 e1) = let t0 = checkExpr ctx e0
                                      t1 = checkExpr ctx e1
                                  in if isScalar t1
                                        then t0
                                        else combine t0 t1
checkExpr ctx (AST.Hash e0 e1)  = let t0 = checkExpr ctx e0
                                      t1 = checkExpr ctx e1
                                  in Check.concat t0 t1
checkExpr ctx (AST.Period e p)  = let t = checkExpr ctx e
                                  in skipPairF p t
checkExpr ctx (AST.Caret e p)   = let t = checkExpr ctx e
                                  in swapPairF p t
