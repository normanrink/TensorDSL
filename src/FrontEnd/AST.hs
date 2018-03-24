
module AST ( Tuple
           , makeTuple
           , Pair
           , makePair
           , Program
           , decls
           , stmts
           , makeProgram
           , DeclRecord
           , makeDeclRecord
           , Decl
           , makeVarDecl
           , makeTypDecl
           , isVarDecl
           , isTypDecl
           , declName
           , declTuple
           , Stmt
           , var
           , expr
           , makeStmt
           , Expr (..)
           , makeId
           , makePlus
           , makeMinus
           , makeStar
           , makeSlash
           , makeHash
           , makePeriod
           , makeCaret
           , prettyPrintProgram
           , prettyPrintDecl
           , prettyPrintStmt
           , prettyPrintExpr
           , prettyPrintExprTopLevel
           , show ) where

import Text.PrettyPrint


type Tuple = [Int]

makeTuple :: [Int] -> Tuple
makeTuple = \x -> x


type Pair  = (Int, Int)

makePair :: Int -> Int -> Pair
makePair a b = (a, b)


data Program = P { decls :: [Decl]
                 , stmts :: [Stmt] }
             deriving (Eq)

makeProgram :: [Decl] -> [Stmt] -> Program
makeProgram = P


type DeclRecord = (String, Tuple)

makeDeclRecord :: String -> Tuple -> DeclRecord
makeDeclRecord s t = (s, t)

data Decl = Dvar DeclRecord
          | Dtyp DeclRecord
          deriving (Eq)

makeVarDecl :: DeclRecord -> Decl
makeVarDecl = Dvar

makeTypDecl :: DeclRecord -> Decl
makeTypDecl = Dtyp

isVarDecl :: Decl -> Bool
isVarDecl (Dvar _) = True
isVarDecl _        = False

isTypDecl :: Decl -> Bool
isTypDecl (Dtyp _) = True
isTypDecl _        = False

declName :: Decl -> String
declName (Dvar (name, _)) = name
declName (Dtyp (name, _)) = name

declTuple :: Decl -> Tuple
declTuple (Dvar (_, tuple)) = tuple
declTuple (Dtyp (_, tuple)) = tuple


data Stmt = S { var  :: String
              , expr :: Expr }
          deriving (Eq)

makeStmt :: String -> Expr -> Stmt
makeStmt = S


data Expr = Id String
          | Plus   Expr Expr
          | Minus  Expr Expr
          | Star   Expr Expr
          | Slash  Expr Expr
          | Hash   Expr Expr
          | Period Expr Pair
          | Caret  Expr Pair
          deriving (Eq)

makeId :: String -> Expr
makeId = Id

makePlus :: Expr -> Expr -> Expr
makePlus = Plus

makeMinus :: Expr -> Expr -> Expr
makeMinus = Minus

makeStar :: Expr -> Expr -> Expr
makeStar = Star

makeSlash :: Expr -> Expr -> Expr
makeSlash = Slash

makeHash :: Expr -> Expr -> Expr
makeHash = Hash

makePeriod :: Expr -> Pair -> Expr
makePeriod = Period

makeCaret :: Expr -> Pair -> Expr
makeCaret = Caret


prettyPrintPair :: Pair -> Doc
prettyPrintPair (x0, x1) = brackets $ (text $ show x0) <+> (text $ show x1)
  
prettyPrintTuple :: Tuple -> Doc
prettyPrintTuple xs = brackets . hsep $ map (text . show) xs


instance Show Expr where
  show = render . prettyPrintExprTopLevel
  
prettyPrintExprTopLevel :: Expr -> Doc
prettyPrintExprTopLevel (Id string)   = text string
prettyPrintExprTopLevel (Plus e0 e1)  =
  let op = (text "+")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (Minus e0 e1) =
  let op = (text "-")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (Star e0 e1)  =
  let op = (text "*")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (Slash e0 e1) =
  let op = (text "/")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (Hash e0 e1)  =
  let op = (text "#")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (Period e p)  =
  let op = (text ".")
  in (prettyPrintExpr e) <> op <> (prettyPrintPair p)
prettyPrintExprTopLevel (Caret e p)   =
  let op = (text "^")
  in (prettyPrintExpr e) <> op <> (prettyPrintPair p)

prettyPrintExpr :: Expr -> Doc
prettyPrintExpr e@(Id _)       = prettyPrintExprTopLevel e
prettyPrintExpr e@(Plus _ _)   = parens $ prettyPrintExprTopLevel e
prettyPrintExpr e@(Minus _ _)  = parens $ prettyPrintExprTopLevel e
prettyPrintExpr e@(Star _ _)   = prettyPrintExprTopLevel e
prettyPrintExpr e@(Slash _ _)  = prettyPrintExprTopLevel e
prettyPrintExpr e@(Hash _ _)   = parens $ prettyPrintExprTopLevel e
prettyPrintExpr e@(Period _ _) = parens $ prettyPrintExprTopLevel e
prettyPrintExpr e@(Caret _ _)  = parens $ prettyPrintExprTopLevel e


instance Show Stmt where
  show = render . prettyPrintStmt
  
prettyPrintStmt :: Stmt -> Doc
prettyPrintStmt (S var expr) = let eqop  = (text "=")
                                   expr' = prettyPrintExprTopLevel expr
                               in (text var) <+> eqop <+> expr'


instance Show Decl where
  show = render . prettyPrintDecl
  
prettyPrintDecl :: Decl -> Doc
prettyPrintDecl (Dvar (id, dims)) = (text "var") <+> (text id) <+>
                                    (text ":") <+> prettyPrintTuple dims
prettyPrintDecl (Dtyp (id, dims)) = (text "type") <+> (text id) <+>
                                    (text ":") <+> prettyPrintTuple dims


instance Show Program where
  show = render . prettyPrintProgram
  
prettyPrintProgram :: Program -> Doc
prettyPrintProgram (P decls stmts) = (vcat $ map prettyPrintDecl decls) $$
                                     space $$
                                     (vcat $ map prettyPrintStmt stmts)
