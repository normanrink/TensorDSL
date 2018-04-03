
module LiftPeriods ( allPeriods
                   , liftPeriod
                   , liftPeriodsInStmt
                   , liftPeriodsInProgram'
                   , liftPeriodsInProgram ) where

import Data.List
import Data.Ord
import Control.Monad.State

import qualified AST
import qualified Check
import qualified Context
import qualified ExprZipper
import qualified Symbol
import qualified TType


type EZipper = ExprZipper.ExprZipper

exprType :: Context.Context -> AST.Expr -> TType.TType
exprType ctx e = let mtype = Check.checkExpr ctx e
                 in case mtype of
                    Just t -> t
                    Nothing -> error "lifting of periods encountered ill-typed expression"

allPeriods :: Int -> EZipper -> [(Int, EZipper)]
allPeriods level ez =
  let goLeft      = ExprZipper.goLeft
      goRight     = ExprZipper.goRight
      goDown      = ExprZipper.goDown
      allPeriods' = allPeriods (level+1)
  in case ExprZipper.expr ez of
    AST.InE (AST.Id _)       -> []
    AST.InE (AST.Plus _ _)   -> (allPeriods' $ goLeft ez) ++ (allPeriods' $ goRight ez)
    AST.InE (AST.Minus _ _)  -> (allPeriods' $ goLeft ez) ++ (allPeriods' $ goRight ez)
    AST.InE (AST.Star _ _)   -> (allPeriods' $ goLeft ez) ++ (allPeriods' $ goRight ez)
    AST.InE (AST.Slash _ _)  -> (allPeriods' $ goLeft ez) ++ (allPeriods' $ goRight ez)
    AST.InE (AST.Hash _ _)   -> (allPeriods' $ goLeft ez) ++ (allPeriods' $ goRight ez)
    AST.InE (AST.Period _ _) -> (level, ez):(allPeriods' $ goDown ez)
    AST.InE (AST.Caret _ _)  -> allPeriods' $ goDown ez
                     
liftPeriod :: EZipper -> State Context.Context (AST.Stmt, AST.Expr)
liftPeriod ez = let contr = ExprZipper.expr ez
                in case contr of
                   AST.InE (AST.Period _ _) ->
                     do ctx <- get
                        var   <- Context.freshVariable
                        let sym    = Symbol.makeVarSymbol var (exprType ctx contr)
                        Context.insertSymbol var sym
                        let assign = AST.makeStmt var contr
                        let varId  = AST.makeId var
                        let expr   = ExprZipper.root $ ExprZipper.modify (\_ -> varId) ez
                        return (assign, expr)
                   _ -> undefined

liftPeriodsInStmt :: AST.Stmt -> State Context.Context [AST.Stmt]
liftPeriodsInStmt stmt = let lhs     = AST.var stmt
                             rhs     = AST.expr stmt
                             periods = allPeriods 0 (ExprZipper.makeZipper rhs)
                         in if length periods <= 1
                               then return [stmt]
                               else let p = maximumBy (\(l0,_) -> \(l1,_) -> compare l0 l1)
                                                      periods
                                    in do (contr, rhs') <- liftPeriod $ snd p
                                          let stmt' = AST.makeStmt lhs rhs'
                                          stmts <- liftPeriodsInStmt stmt'
                                          return (contr:stmts)

liftPeriodsInProgram' :: AST.Program -> State Context.Context AST.Program
liftPeriodsInProgram' p = let decls  = AST.decls p
                              stmts  = AST.stmts p
                              folder = \stmts ->
                                       \stmt -> do { stmts' <- liftPeriodsInStmt stmt;
                                                     return $ stmts ++ stmts' }
                          in do stmts' <- foldM folder [] stmts
                                return $ AST.makeProgram decls stmts'

liftPeriodsInProgram :: AST.Program -> Context.Context -> (AST.Program, Context.Context)
liftPeriodsInProgram p ctx = runState (liftPeriodsInProgram' p) ctx 
