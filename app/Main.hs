{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import CLI.Commands
import CLI.Executor
import Options.Applicative

main :: IO ()
main = do
  options <- execParser opts
  executeCommand $ optCommand options