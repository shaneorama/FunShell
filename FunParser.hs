module FunParser where

import System.IO
import Control.Monad
-- import Control.Applicative
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import FunAST

languageDef = emptyDef {
  Token.identStart = letter,
  Token.identLetter = alphaNum,
  Token.reservedOpNames = ["$",".","=","|>"]
}

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
stringLiteral = Token.stringLiteral lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer

arg = liftM Literal stringLiteral <|>
      liftM Id identifier

statement = try funDef <|> funCall

funCall :: Parser Statement
funCall = FunCall <$>
  identifier <*>
  many arg

funDef :: Parser Statement
funDef = FunDef
  <$> identifier
  <*> many identifier
  <* reservedOp "="
  <*> many arg

parseString str =
  case parse statement "" str of
    Left e -> error $ show e
    Right r -> r
