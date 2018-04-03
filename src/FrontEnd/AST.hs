
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
           , ExprF(..)
           , Expr(InE), outE
           , makeId
           , makePlus
           , makeMinus
           , makeStar
           , makeSlash
           , makeHash
           , makePeriod
           , makeCaret
           , periodsInExpr
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


data ExprF a  = Id String
              | Plus   a a
              | Minus  a a
              | Star   a a
              | Slash  a a
              | Hash   a a
              | Period a Pair
              | Caret  a Pair
          deriving (Eq, Show)

instance Functor ExprF where
  fmap f (Id x)        = Id x
  fmap f (Plus e0 e1)  = Plus (f e0) (f e1)
  fmap f (Minus e0 e1) = Minus (f e0) (f e1)
  fmap f (Star e0 e1)  = Star (f e0) (f e1)
  fmap f (Slash e0 e1) = Slash (f e0) (f e1)
  fmap f (Hash e0 e1)  = Hash (f e0) (f e1)
  fmap f (Period e p)  = Period (f e) p
  fmap f (Caret e p)   = Caret (f e) p
    
data Expr = InE { outE :: ExprF Expr }
          deriving (Eq)

makeId :: String -> Expr
makeId = InE . Id

makePlus :: Expr -> Expr -> Expr
makePlus e0 e1 = InE $ Plus e0 e1

makeMinus :: Expr -> Expr -> Expr
makeMinus e0 e1 = InE $ Minus e0 e1

makeStar :: Expr -> Expr -> Expr
makeStar e0 e1 = InE $ Star e0 e1

makeSlash :: Expr -> Expr -> Expr
makeSlash e0 e1 = InE $ Slash e0 e1

makeHash :: Expr -> Expr -> Expr
makeHash e0 e1 = InE $ Hash e0 e1

makePeriod :: Expr -> Pair -> Expr
makePeriod e p = InE $ Period e p

makeCaret :: Expr -> Pair -> Expr
makeCaret e p = InE $ Caret e p


periodsInExpr :: Expr -> Int
periodsInExpr (InE (Plus e0 e1))  = periodsInExpr e0 + periodsInExpr e1
periodsInExpr (InE (Minus e0 e1)) = periodsInExpr e0 + periodsInExpr e1
periodsInExpr (InE (Star e0 e1))  = periodsInExpr e0 + periodsInExpr e1
periodsInExpr (InE (Slash e0 e1)) = periodsInExpr e0 + periodsInExpr e1
periodsInExpr (InE (Hash e0 e1))  = periodsInExpr e0 + periodsInExpr e1
periodsInExpr (InE (Period e0 _)) = 1 + periodsInExpr e0
periodsInExpr (InE (Caret e0 _))  = periodsInExpr e0


prettyPrintPair :: Pair -> Doc
prettyPrintPair (x0, x1) = brackets $ (text $ show x0) <+> (text $ show x1)
  
prettyPrintTuple :: Tuple -> Doc
prettyPrintTuple xs = brackets . hsep $ map (text . show) xs


instance Show Expr where
  show = render . prettyPrintExprTopLevel
  
prettyPrintExprTopLevel :: Expr -> Doc
prettyPrintExprTopLevel (InE (Id string))   = text string
prettyPrintExprTopLevel (InE (Plus e0 e1))  =
  let op = (text "+")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (InE (Minus e0 e1)) =
  let op = (text "-")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (InE (Star e0 e1))  =
  let op = (text "*")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (InE (Slash e0 e1)) =
  let op = (text "/")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (InE (Hash e0 e1))  =
  let op = (text "#")
  in (prettyPrintExpr e0) <> op <> (prettyPrintExpr e1)
prettyPrintExprTopLevel (InE (Period e p))  =
  let op = (text ".")
  in (prettyPrintExpr e) <> op <> (prettyPrintPair p)
prettyPrintExprTopLevel (InE (Caret e p))   =
  let op = (text "^")
  in (prettyPrintExpr e) <> op <> (prettyPrintPair p)

prettyPrintExpr :: Expr -> Doc
prettyPrintExpr e@(InE (Id _))       = prettyPrintExprTopLevel e
prettyPrintExpr e@(InE (Plus _ _))   = parens $ prettyPrintExprTopLevel e
prettyPrintExpr e@(InE (Minus _ _))  = parens $ prettyPrintExprTopLevel e
prettyPrintExpr e@(InE (Star _ _))   = prettyPrintExprTopLevel e
prettyPrintExpr e@(InE (Slash _ _))  = prettyPrintExprTopLevel e
prettyPrintExpr e@(InE (Hash _ _))   = parens $ prettyPrintExprTopLevel e
prettyPrintExpr e@(InE (Period _ _)) = parens $ prettyPrintExprTopLevel e
prettyPrintExpr e@(InE (Caret _ _))  = parens $ prettyPrintExprTopLevel e


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
