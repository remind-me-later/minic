module Main where

import Ast.Parse qualified
import Ast.Semant qualified
import Mir.Translate qualified
import System.Environment qualified
import Text.Parsec qualified

main :: IO ()
main = do
  args <- System.Environment.getArgs
  let fileName = head args
  contents <- readFile fileName

  ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
    Right ast -> return ast
    Left err -> error $ "Parsing failed: " ++ show err

  -- putStrLn "AST:"
  -- print ast

  typedAst <- case Ast.Semant.typeProgram ast of
    Right table -> return table
    Left errs -> error $ "Type checking failed: " ++ show errs

  -- putStrLn "Typed AST:"
  -- print typedAst

  let mirProgram = Mir.Translate.transProgram typedAst
  putStrLn "MIR Program:"
  print mirProgram