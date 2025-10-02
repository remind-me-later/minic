module Main (main) where

import CLI.Commands (Options (optCommand), opts)
import CLI.Executor (executeCommand)
import Options.Applicative (execParser)

main :: IO ()
main = do
  options <- execParser opts
  executeCommand $ optCommand options