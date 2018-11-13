module FooParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BExpr = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less deriving (Show)

data AExpr = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)

data ABinOp = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data Stmt = Seq [Stmt]
  | Assign String AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Skip
  deriving (Show)

languageDef = emptyDef {
  Token.commentStart = "/*",
  Token.commentEnd = "*/",
  Token.identStart = letter,
  Token.identLetter = alphaNum,
  Token.reservedNames = ["if","then","else","while","do","skip","true","false","not","and","or"],
  Token.reservedOpNames = ["+","-","*","/",":=","<",">","and","or","not"]
}

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser = whiteSpace >> statement

statement = parens statement <|> sequenceOfStmt
sequenceOfStmt = do
  list <- (sepBy1 statement' semi)
  return $ if length list == 1 then head list else Seq list

statement' = ifStmt <|> whileStmt <|> skipStmt <|> assignStmt

ifStmt = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  stmt1 <- statement
  reserved "else"
  stmt2 <- statement
  return $ If cond stmt1 stmt2

whileStmt = do
  reserved "while"
  cond <- bExpression
  reserved "do"
  stmt <- statement
  return $ While cond stmt

assignStmt = do
  var <- identifier
  reservedOp ":="
  expr <- aExpression
  return $ Assign var expr

skipStmt = reserved "skip" >> return Skip

aExpression = buildExpressionParser aOperators aTerm
bExpression = buildExpressionParser bOperators bTerm

aOperators = [
  [Prefix (reservedOp "-" >> return (Neg))],
  [Infix (reservedOp "*" >> return (ABinary Multiply)) AssocLeft, Infix (reservedOp "/" >> return (ABinary Divide)) AssocLeft],
  [Infix (reservedOp "+" >> return (ABinary Add)) AssocLeft, Infix (reservedOp "-" >> return (ABinary Subtract)) AssocLeft] ]

bOperators = [
  [Prefix (reservedOp "not" >> return (Not))],
  [Infix (reservedOp "and" >> return (BBinary And)) AssocLeft, Infix (reservedOp "or" >> return (BBinary Or)) AssocLeft] ]

aTerm = parens aExpression <|> liftM Var identifier <|> liftM IntConst integer
bTerm = parens bExpression <|> (reserved "true" >> return (BoolConst True)) <|> (reserved "false" >> return (BoolConst False)) <|> rExpression

rExpression = do
  a1 <- aExpression
  op <- relation
  a2 <- aExpression
  return $ RBinary op a1 a2

relation = (reservedOp ">" >> return Greater) <|> (reservedOp "<" >> return Less)

parseString str =
  case parse whileParser "" str of
    Left e -> error $ show e
    Right r -> r
 
