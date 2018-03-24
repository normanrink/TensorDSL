
module Symbol (Symbol
              , makeVarSymbol
              , makeTypSymbol
              , isVarSymbol
              , isTypSymbol
              , symbolName
              , symbolType
              , declSymbol ) where

import qualified AST
import qualified TType


data Symbol = VarS String TType.TType
            | TypS String TType.TType
            deriving (Eq, Show)

makeVarSymbol :: String -> TType.TType -> Symbol
makeVarSymbol = VarS

makeTypSymbol :: String -> TType.TType -> Symbol
makeTypSymbol = TypS

isVarSymbol :: Symbol -> Bool
isVarSymbol (VarS _ _) = True
isVarSymbol _          = False

isTypSymbol :: Symbol -> Bool
isTypSymbol (TypS _ _) = True
istypSymbol _          = False

symbolName :: Symbol -> String
symbolName (VarS name _) = name
symbolName (TypS name _) = name

symbolType :: Symbol -> TType.TType
symbolType (VarS _ ttype) = ttype
symbolType (TypS _ ttype) = ttype

declSymbol :: AST.Decl -> Symbol
declSymbol decl | AST.isVarDecl decl = makeVarSymbol (AST.declName decl)
                                                     (AST.declTuple decl)
                | AST.isTypDecl decl = makeTypSymbol (AST.declName decl)
                                                     (AST.declTuple decl)
                | otherwise          = error "invalid declaration"
