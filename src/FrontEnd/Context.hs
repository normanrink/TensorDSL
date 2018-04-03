
module Context ( Context
               , emptyCtx
               , inScope
               , getSymbol
               , insertDecl
               , insertDecls
               , fromDecls
               , freshVariable
               , insertSymbol ) where


import Control.Monad.State
import qualified Data.Map as Map

import qualified AST
import qualified Symbol
import qualified TType


data Context = Ctx { table :: Map.Map String Symbol.Symbol
                   , vars  :: Int }
             deriving (Eq, Show)

emptyCtx :: Context
emptyCtx = Ctx Map.empty 0

inScope :: String -> Context -> Bool
inScope name ctx = Map.member name (table ctx)

lookup :: String -> Context -> Maybe Symbol.Symbol
lookup name ctx = Map.lookup name (table ctx)

insert :: String -> Symbol.Symbol -> Context -> Context
insert name sym (Ctx table vars) = Ctx (Map.insert name sym table) vars 

incVars :: Context -> Context
incVars (Ctx table vars) = Ctx table (vars+1)


getSymbol :: String -> Context -> Symbol.Symbol
getSymbol name ctx = case Context.lookup name ctx of
  Nothing  -> error "name not bound in context"
  Just sym -> sym

getType :: String -> Context -> TType.TType
getType name ctx = Symbol.symbolType $ getSymbol name ctx

makeSymbol :: AST.Decl -> String -> TType.TType -> Symbol.Symbol
makeSymbol decl name tuple | AST.isVarDecl decl =
                               Symbol.makeVarSymbol name tuple
                           | AST.isTypDecl decl =
                               Symbol.makeTypSymbol name tuple
                           | otherwise = undefined
                             
insertDecl :: Context -> AST.Decl -> Context
insertDecl ctx decl = let name  = AST.declName decl
                          tuple = if AST.isDeclWithString decl
                                     then getType (AST.declString decl) ctx
                                     else AST.declTuple decl
                          sym   = makeSymbol decl name tuple
                      in if inScope name ctx
                            then error "re-declaration"
                            else insert name sym ctx

insertDecls :: Context -> [AST.Decl] -> Context
insertDecls ctx decl = foldl insertDecl ctx decl

fromDecls :: [AST.Decl] -> Context
fromDecls = insertDecls emptyCtx

freshVariable :: State Context String
freshVariable = do ctx <- get
                   put $ incVars ctx
                   return $ "_t" ++ show (vars ctx)

insertSymbol :: String -> Symbol.Symbol -> State Context ()
insertSymbol name sym = do ctx <- get
                           put $ insert name sym ctx
