
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


ioQualParserNone :: Parsec.Parsec String () String
ioQualParserNone = return $ "none"

ioQualParser :: Parsec.Parsec String () AST.IOQualifier
ioQualParser = do
  qs <- (Parsec.try $ stringParser "none") Parsec.<|>
        (Parsec.try $ stringParser "in") Parsec.<|>
        (Parsec.try $ stringParser "out") Parsec.<|>
        (Parsec.try $ stringParser "inout") Parsec.<|>
        ioQualParserNone
  return $ AST.ioQualifier qs
  
declTupleParser :: AST.IOQualifier -> Parsec.Parsec String () AST.DeclRecord
declTupleParser q = do
  id <- idParser
  charParser ':'
  dims <- tupleParser
  return $ AST.makeDeclRecordFromTuple id dims

declStringParser :: AST.IOQualifier -> Parsec.Parsec String () AST.DeclRecord
declStringParser q = do
  id <- idParser
  charParser ':'
  string <- idParser
  return $ AST.makeDeclRecordFromString id string

declParser' :: String -> Parsec.Parsec String () (AST.DeclRecord, AST.IOQualifier)
declParser' keyword = do
  stringParser keyword
  q  <- ioQualParser
  dr <- (Parsec.try $ declTupleParser q) Parsec.<|> declStringParser q
  return (dr, q)
  
varDeclParser :: Parsec.Parsec String () AST.Decl
varDeclParser = do
  (dr, q) <- declParser' "var"
  return $ AST.makeVarDecl dr q

typDeclParser :: Parsec.Parsec String () AST.Decl
typDeclParser = do
  (dr, q) <- declParser' "type"
  return $ AST.makeTypDecl dr q

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
