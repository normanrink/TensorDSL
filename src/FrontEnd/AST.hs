
module AST ( Tuple
           , makeTuple
           , Pair
           , makePair
           , Program
           , decls
           , stmts
           , makeProgram
           , DeclRecord
           , makeDeclRecordFromTuple, makeDeclRecordFromString
           , isTupleDeclRecord, isStringDeclRecord
           , Decl
           , makeVarDecl, makeTypDecl
           , isVarDecl, isTypDecl
           , declName, declTuple, declString
           , isDeclWithTuple, isDeclWithString
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


data DeclRecord = DRtuple String Tuple
                | DRstring String String
                deriving (Eq)

makeDeclRecordFromTuple :: String -> Tuple -> DeclRecord
makeDeclRecordFromTuple s t = DRtuple s t

makeDeclRecordFromString :: String -> String -> DeclRecord
makeDeclRecordFromString s t = DRstring s t

isTupleDeclRecord :: DeclRecord -> Bool
isTupleDeclRecord (DRtuple _ _) = True
isTupleDeclRecord _             = False

isStringDeclRecord :: DeclRecord -> Bool
isStringDeclRecord (DRstring _ _) = True
isStringDeclRecord _              = False

declRecordName :: DeclRecord -> String
declRecordName (DRtuple name _)  = name
declRecordName (DRstring name _) = name

declRecordTuple :: DeclRecord -> Tuple
declRecordTuple (DRtuple _ tuple) = tuple
declrecordTuple _                 = undefined

declRecordString :: DeclRecord -> String
declRecordString (DRstring _ string) = string
declRecordString _                   = undefined


data Decl = Dvar { record :: DeclRecord }
          | Dtyp { record :: DeclRecord }
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
declName = declRecordName . record 

declTuple :: Decl -> Tuple
declTuple = declRecordTuple .record

declString :: Decl -> String
declString = declRecordString . record

isDeclWithTuple :: Decl -> Bool
isDeclWithTuple = isTupleDeclRecord . record

isDeclWithString :: Decl -> Bool
isDeclWithString = isStringDeclRecord . record


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
prettyPrintDecl (Dvar dr) = (text "var") <+> prettyPrintDeclRecord dr
prettyPrintDecl (Dtyp dr) = (text "type") <+> prettyPrintDeclRecord dr


instance Show Program where
  show = render . prettyPrintProgram
  
prettyPrintProgram :: Program -> Doc
prettyPrintProgram (P decls stmts) = (vcat $ map prettyPrintDecl decls) $$
                                     space $$
                                     (vcat $ map prettyPrintStmt stmts)


instance Show DeclRecord where
  show = render . prettyPrintDeclRecord

prettyPrintDeclRecord :: DeclRecord -> Doc
prettyPrintDeclRecord (DRtuple  name dims)   = (text name) <+> (text ":") <+>
                                               prettyPrintTuple dims
prettyPrintDeclRecord (DRstring name string) = (text name) <+> (text ":") <+>
                                               (text string)

