module FunShell where

import System.IO
import FunParser
import FunAST

execute (FunCall name args) = "calling: "++name++" with args: "++show args
execute (FunDef name params args) = "defining: "++name++" with params: "++show params++" with args: "++show args

main = do
  putStr "|> "
  statement <- getLine
  putStrLn $ execute $ parseString statement
  main
