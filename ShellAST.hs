module ShellAST where

data Arg = Id String | Literal String
  deriving (Show, Read)

data Param = FromFunc FuncCall | FromInput String
  deriving (Show, Read)

data FuncCall = FuncCall String [Arg]
  deriving (Show, Read)

data FuncDef = FuncDef String [Param] FuncCall
  deriving (Show, Read)

data Statement = Call FuncCall | Def FuncDef
  deriving (Show, Read)
