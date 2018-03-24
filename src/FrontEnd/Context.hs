
 -- deliberately export every function defined in this file:
module Context where


import qualified Data.Map as Map

import qualified Symbol
import qualified AST


type Context = Map.Map String Symbol.Symbol


emptyCtx :: Context
emptyCtx = Map.empty

inScope :: String -> Context -> Bool
inScope = Map.member

lookup :: String -> Context -> Maybe Symbol.Symbol
lookup = Map.lookup

getSymbol :: String -> Context -> Symbol.Symbol
getSymbol name ctx = case Context.lookup name ctx of
  Nothing  -> error "name not bound in context"
  Just sym -> sym

insert :: String -> Symbol.Symbol -> Context -> Context
insert = Map.insert

insertDecl :: Context -> AST.Decl -> Context
insertDecl ctx decl = let name = AST.declName decl
                          sym  = Symbol.declSymbol decl
                      in if inScope name ctx
                            then error "re-declaration"
                            else insert name sym ctx

insertDecls :: Context -> [AST.Decl] -> Context
insertDecls ctx decl = foldl insertDecl ctx decl

fromDecls :: [AST.Decl] -> Context
fromDecls = insertDecls emptyCtx
