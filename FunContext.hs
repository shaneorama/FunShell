module FunContext where

import FunAST

class ReturnType rt = ReturnType rt

data Fun = Fun String [String] [Arg]

data FunContext = FunContext {
    funs :: String -> [String] -> [Arg] -> ResultType
}

execute FunContext (FunCall name args) = "calling: "++name++" with args: "++show args
execute FunContext (FunDef name params args) = "defining: "++name++" with params: "++show params++" with args: "++show args
