module FunAST where

data Arg = Id String | Literal String | FromInput String
  deriving (Show, Read)

data Statement = FunCall String [Arg] | FunDef String [String] [Arg]
  deriving (Show, Read)
