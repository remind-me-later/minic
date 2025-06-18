module Main where

import Ir (programToMir)
import Parser (parseProgram)
import ParserCombinators (Parser (parse))
import System.Environment (getArgs)
import Token (tokens)
import TypeCheck (typeProgram)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName

  toks <- case parse tokens contents of
    Just (t, _) -> return t
    Nothing -> error "Failed to parse tokens"

  putStrLn "Tokens:"
  print toks

  ast <- case parse parseProgram toks of
    Just (p, _) -> return p
    Nothing -> error "Failed to parse AST"

  putStrLn "AST:"
  print ast

  typedAst <- case typeProgram ast of
    Right table -> return table
    Left err -> error $ "Type checking failed: " ++ err

  -- putStrLn "Typed AST:"
  -- print typedAst

  let mirProgram = programToMir typedAst
  putStrLn "MIR Program:"
  print mirProgram