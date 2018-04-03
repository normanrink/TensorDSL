
module Parse ( parseProgram ) where

import qualified Text.Parsec as Parsec

import qualified AST


-- The 'tokenParser' skips trailing whitespace:
tokenParser :: Parsec.Parsec String u a -> Parsec.Parsec String u a
tokenParser p = do
  result <- p
  Parsec.spaces
  return result

charParser :: Char -> Parsec.Parsec String u Char
charParser char = tokenParser $ Parsec.char char

stringParser :: String -> Parsec.Parsec String u String
stringParser string = tokenParser (do s <- Parsec.string string
                                      Parsec.space
                                      return s)

intParser :: Parsec.Parsec String () Int
intParser = tokenParser (do digits <- Parsec.many1 Parsec.digit
                            return $ read digits)

idParser :: Parsec.Parsec String () String
idParser = tokenParser (do start <- Parsec.letter
                           rest  <- Parsec.many Parsec.alphaNum
                           return (start:rest))


tupleParser :: Parsec.Parsec String () AST.Tuple
tupleParser = do
  charParser '['
  ints <- Parsec.many intParser
  charParser ']'
  return $ AST.makeTuple ints

pairParser :: Parsec.Parsec String () AST.Pair
pairParser = do
  charParser '['
  i0 <- intParser
  i1 <- intParser
  charParser ']'
  return $ AST.makePair i0 i1


declTupleParser :: Parsec.Parsec String () AST.DeclRecord
declTupleParser = do
  id <- idParser
  charParser ':'
  dims <- tupleParser
  return $ AST.makeDeclRecordFromTuple id dims

declStringParser :: Parsec.Parsec String () AST.DeclRecord
declStringParser = do
  id <- idParser
  charParser ':'
  string <- idParser
  return $ AST.makeDeclRecordFromString id string

declParser' :: String -> Parsec.Parsec String () AST.DeclRecord
declParser' keyword = do
  stringParser keyword
  (Parsec.try declTupleParser) Parsec.<|> declStringParser
  
varDeclParser :: Parsec.Parsec String () AST.Decl
varDeclParser = do
  d <- declParser' "var"
  return $ AST.makeVarDecl d

typDeclParser :: Parsec.Parsec String () AST.Decl
typDeclParser = do
  d <- declParser' "type"
  return $ AST.makeTypDecl d

declParser :: Parsec.Parsec String () AST.Decl
declParser = varDeclParser Parsec.<|> typDeclParser


opParser :: Char -> a -> Parsec.Parsec String () a
opParser op maker = charParser op >> return maker

addopParser :: Parsec.Parsec String () (AST.Expr -> AST.Expr -> AST.Expr)
addopParser = (opParser '+' AST.makePlus) Parsec.<|> (opParser '-' AST.makeMinus)

mulopParser :: Parsec.Parsec String () (AST.Expr -> AST.Expr -> AST.Expr)
mulopParser = (opParser '*' AST.makeStar) Parsec.<|> (opParser '/' AST.makeSlash)

hashParser :: Parsec.Parsec String () (AST.Expr -> AST.Expr -> AST.Expr)
hashParser = opParser '#' AST.makeHash

periodParser :: Parsec.Parsec String () (AST.Expr -> AST.Pair -> AST.Expr)
periodParser = opParser '.' AST.makePeriod

caretParser :: Parsec.Parsec String () (AST.Expr -> AST.Pair -> AST.Expr)
caretParser = opParser '^' AST.makeCaret

parenExprParser :: Parsec.Parsec String () AST.Expr
parenExprParser = do
  charParser '('
  e <- exprParser
  charParser ')'
  return e

idExprParser :: Parsec.Parsec String () AST.Expr
idExprParser = do
  id <- idParser
  return $ AST.makeId id
  
exprParser :: Parsec.Parsec String () AST.Expr
exprParser = Parsec.chainl1 exprParser0 addopParser

exprParser0 :: Parsec.Parsec String () AST.Expr
exprParser0 = Parsec.chainl1 exprParser1 mulopParser

exprParser1 :: Parsec.Parsec String () AST.Expr
exprParser1 = Parsec.chainl1 exprParser2 hashParser

exprParser2 :: Parsec.Parsec String () AST.Expr
exprParser2 = do
  e <- exprParser3
  pairopParser e

pairopParser :: AST.Expr -> Parsec.Parsec String () AST.Expr
pairopParser e = Parsec.option e (do maker <- periodParser Parsec.<|> caretParser
                                     p <- pairParser
                                     pairopParser $ maker e p)

exprParser3 :: Parsec.Parsec String () AST.Expr
exprParser3 = idExprParser Parsec.<|> parenExprParser


stmtParser :: Parsec.Parsec String () AST.Stmt
stmtParser = do
  id <- idParser
  charParser '='
  e <- exprParser
  return $ AST.makeStmt id e

programParser :: Parsec.Parsec String () AST.Program
programParser = do
  -- throw away leading whitespace:
  Parsec.spaces
  decls <- Parsec.endBy declParser Parsec.spaces
  stmts <- Parsec.endBy stmtParser Parsec.spaces
  return $ AST.makeProgram decls stmts

parseProgram :: String -> AST.Program
parseProgram s = let result = Parsec.parse programParser "" s
                 in case result of
                    Left  err -> error $ show err
                    Right ast -> ast
