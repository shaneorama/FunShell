module FunShell where

import System.Console.Readline
import System.IO
import FunParser
import FunAST


main = do
  maybeLine <- readline "|> "
  case maybeLine of
    Nothing -> return ()
    Just "exit" -> return ()
    Just "" -> main
    Just line -> do
      addHistory line
      putStrLn $ execute $ parseString line
      main
