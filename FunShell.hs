module FunShell where

import System.IO
import ShellParser

main = do
  putStr "|> "
  input <- readLn
  putStrLn $ show $ parseString input
  main
