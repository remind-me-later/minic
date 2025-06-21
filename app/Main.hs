module Main where

import Ast.Parse qualified
import Ast.Semant qualified
import Mir.Translate qualified
import System.Environment qualified
import Text.Parsec qualified
import X86.Translate qualified

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["--ast", fileName] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      print typedAst
    ["--semant", fileName] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      print typedAst
    ["--mir", fileName] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      let mirProgram = Mir.Translate.transProgram typedAst
      print mirProgram
    ["--x86", fileName] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      let mirProgram = Mir.Translate.transProgram typedAst
      let x86Program = X86.Translate.translateProgram mirProgram
      putStrLn x86Program
    _ -> putStrLn "Usage: compiler --mir <file> | --x86 <file> | --ast <file> | --semant <file>"