module Main where

import Parser (program)
import ParserCombinators (Parser (parse))
import System.Environment (getArgs)
import Token (tokens)
import TypeCheck (typeCheck)

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

  ast <- case parse program toks of
    Just (p, _) -> return p
    Nothing -> error "Failed to parse AST"

  putStrLn "AST:"
  print ast

  symbolTable <- case typeCheck ast of
    Right table -> return table
    Left err -> error $ "Type checking failed: " ++ err

  putStrLn "Symbol Table:"
  print symbolTable