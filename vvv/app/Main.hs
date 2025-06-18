module Main where

import Ir (programToMir)
import Parser qualified (program)
import System.Environment (getArgs)
import Text.Parsec qualified as Parsec
import TypeCheck (typeProgram)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName

  ast <- case Parsec.parse Parser.program fileName contents of
    Right ast -> return ast
    Left err -> error $ "Parsing failed: " ++ show err

  -- putStrLn "AST:"
  -- print ast

  typedAst <- case typeProgram ast of
    Right table -> return table
    Left err -> error $ "Type checking failed: " ++ err

  putStrLn "Typed AST:"
  print typedAst

  let mirProgram = programToMir typedAst
  putStrLn "MIR Program:"
  print mirProgram