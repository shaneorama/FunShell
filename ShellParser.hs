module ShellParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import ShellAST

languageDef = emptyDef {
  Token.identStart = letter,
  Token.identLetter = alphaNum,
  Token.reservedOpNames = ["=", "|>"]
}

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
stringLiteral = Token.stringLiteral lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer


arg = liftM Literal stringLiteral <|> liftM Id identifier
param = liftM FromFunc funcCall <|> liftM FromInput stringLiteral

funcCall :: Parser FuncCall
funcCall = do
  name <- identifier
  args <- many arg
  return $ FuncCall name args

funcDef :: Parser FuncDef
funcDef = do
  name <- identifier
  params <- many param
  reservedOp "="
  func <- funcCall
  return $ FuncDef name params func

statement = liftM Def funcDef <|> liftM Call funcCall

parseString str =
  case parse statement "" str of
    Left e -> error $ show e
    Right r -> r
