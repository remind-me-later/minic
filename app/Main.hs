module Main where

import Ast.Parse qualified
import Ast.Semant qualified
import Mir.Allocation qualified as Allocation
import Mir.Liveness qualified as Liveness
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

      print ast
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
    ["--mir", fileName, "--live"] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      let mirProgram = Mir.Translate.transProgram typedAst
      let livenessInfo = Liveness.analyzeProgramLiveness mirProgram
      print mirProgram
      putStrLn "Liveness Information:"
      print livenessInfo
    ["--mir", fileName, "--color"] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      let mirProgram = Mir.Translate.transProgram typedAst
      let allocationResult = Allocation.allocateProgram mirProgram
      print allocationResult
    ["--mir", fileName, "-o", outFile] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      let mirProgram = Mir.Translate.transProgram typedAst
      writeFile outFile (show mirProgram)
    ["--x86", fileName] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      let mirProgram = Mir.Translate.transProgram typedAst
      let allocationResult = Allocation.allocateProgram mirProgram
      let x86Program = X86.Translate.translateProgram allocationResult
      putStrLn x86Program
    ["--x86", fileName, "-o", outFile] -> do
      contents <- readFile fileName
      ast <- case Text.Parsec.parse Ast.Parse.program fileName contents of
        Right ast -> return ast
        Left err -> error $ "Parsing failed: " ++ show err

      typedAst <- case Ast.Semant.typeProgram ast of
        Right table -> return table
        Left errs -> error $ "Type checking failed: " ++ show errs

      let mirProgram = Mir.Translate.transProgram typedAst
      let allocationResult = Allocation.allocateProgram mirProgram
      let x86Program = X86.Translate.translateProgram allocationResult
      writeFile outFile x86Program
    _ -> putStrLn "Usage: compiler --mir <file> [-o <outfile>] | --x86 <file> [-o <outfile>] | --ast <file> | --semant <file>"