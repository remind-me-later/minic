{-# LANGUAGE LambdaCase #-}

module Main where

import Parser (program)
import ParserCombinators (Parser (parse))
import System.Environment (getArgs)
import Token (tokens)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  let tokens' = parse tokens contents
  putStrLn "Tokens:"
  print tokens'

  putStrLn "\nAST:"
  print $ (\case Just (t, _) -> parse program t; _ -> Nothing) tokens'