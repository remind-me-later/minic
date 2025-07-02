{-# LANGUAGE ApplicativeDo #-}

module Main where

import Ast.Parse qualified
import Ast.Semant qualified
import Ast.Types qualified
import Data.Map qualified as Map
import Mir qualified as Mir.CopyPropagation
import Mir.Allocation qualified as Allocation
import Mir.Interference qualified as Interference
import Mir.Liveness qualified as Liveness
import Mir.Translate qualified
import Mir.Types qualified
import Options.Applicative
import Text.Parsec qualified
import X86.Translate qualified

data Command
  = ShowAst String
  | ShowSemant String
  | ShowMir String
  | ShowMirLive String
  | ShowMirInterference String
  | ShowMirColor String
  | ShowMirOptimized String
  | MirToFile String String
  | ShowX86 String
  | X86ToFile String String

newtype Options = Options
  {optCommand :: Command}

-- Parser for commands
commandParser :: Parser Command
commandParser =
  subparser
    ( command "ast" (info astParser (progDesc "Parse and show AST"))
        <> command "semant" (info semantParser (progDesc "Type check and show typed AST"))
        <> command "mir" (info mirParser (progDesc "Generate MIR"))
        <> command "x86" (info x86Parser (progDesc "Generate x86 assembly"))
    )

astParser :: Parser Command
astParser = ShowAst <$> strArgument (metavar "FILE" <> help "Input file")

semantParser :: Parser Command
semantParser = ShowSemant <$> strArgument (metavar "FILE" <> help "Input file")

mirParser :: Parser Command
mirParser = do
  file <- strArgument (metavar "FILE" <> help "Input file")
  liveFlag <- switch (long "live" <> help "Show liveness analysis")
  colorFlag <- switch (long "color" <> help "Show register allocation")
  interferenceFlag <- switch (long "interference" <> help "Show interference graph")
  optimizedFlag <- switch (long "opt" <> help "Show optimized MIR")
  outFile <- optional (strOption (short 'o' <> metavar "OUTFILE" <> help "Output file"))

  pure $ case (liveFlag, colorFlag, outFile, interferenceFlag, optimizedFlag) of
    (True, _, _, _, _) -> ShowMirLive file
    (_, True, _, _, _) -> ShowMirColor file
    (_, _, Just out, _, _) -> MirToFile file out
    (_, _, _, True, _) -> ShowMirInterference file
    (_, _, _, _, True) -> ShowMirOptimized file
    _ -> ShowMir file

x86Parser :: Parser Command
x86Parser = do
  file <- strArgument (metavar "FILE" <> help "Input file")
  outFile <- optional (strOption (short 'o' <> metavar "OUTFILE" <> help "Output file"))

  pure $ case outFile of
    Just out -> X86ToFile file out
    Nothing -> ShowX86 file

opts :: ParserInfo Options
opts =
  info
    (Options <$> commandParser <**> helper)
    ( fullDesc
        <> progDesc "Compiler for minic language"
        <> header "minic-compiler - a compiler for the minic programming language"
    )

main :: IO ()
main = do
  options <- execParser opts
  executeCommand $ optCommand options

-- Extract common parsing logic
parseFile :: String -> IO (Either String Ast.Types.RawProgram)
parseFile fileName = do
  contents <- readFile fileName
  pure $ case Text.Parsec.parse Ast.Parse.program fileName contents of
    Right ast -> Right ast
    Left err -> Left $ "Parsing failed: " ++ show err

typeCheckAst :: Ast.Types.RawProgram -> IO (Either String Ast.Semant.TypedProgram)
typeCheckAst ast = pure $ case Ast.Semant.typeProgram ast of
  Right table -> Right table
  Left errs -> Left $ "Type checking failed: " ++ show errs

-- Execute commands with shared logic
executeCommand :: Command -> IO ()
executeCommand cmd = case cmd of
  ShowAst fileName -> do
    result <- parseFile fileName
    case result of
      Right ast -> print ast
      Left err -> error err
  ShowSemant fileName -> do
    result <- parseFile fileName
    case result of
      Right ast -> do
        typedResult <- typeCheckAst ast
        case typedResult of
          Right typedAst -> print typedAst
          Left err -> error err
      Left err -> error err
  ShowMir fileName -> do
    processedAst <- processToMir fileName
    case processedAst of
      Right mirProgram -> print mirProgram
      Left err -> error err
  ShowMirLive fileName -> do
    processedAst <- processToMir fileName
    case processedAst of
      Right mirProgram -> do
        let livenessInfo = Liveness.analyzeProgramLiveness mirProgram
        print mirProgram
        putStrLn "Liveness Information:"
        print livenessInfo
      Left err -> error err
  ShowMirColor fileName -> do
    processedAst <- processToMir fileName
    case processedAst of
      Right mirProgram -> do
        let allocationResult = Allocation.allocateProgram mirProgram
        let livenessInfo = Liveness.analyzeProgramLiveness mirProgram
        print mirProgram
        putStrLn "Liveness Information:"
        print livenessInfo
        putStrLn "Allocation Result:"
        print allocationResult
      Left err -> error err
  ShowMirOptimized fileName -> do
    processedAst <- processToMir fileName
    case processedAst of
      Right mirProgram -> do
        let optimizedProgram = Mir.CopyPropagation.optimizeProgram mirProgram
        print optimizedProgram
      Left err -> error err
  MirToFile fileName outFile -> do
    processedAst <- processToMir fileName
    case processedAst of
      Right mirProgram -> writeFile outFile (show mirProgram)
      Left err -> error err
  ShowMirInterference fileName -> do
    processedAst <- processToMir fileName
    case processedAst of
      Right mirProgram -> do
        print mirProgram
        let interferenceGraph = Interference.programInterferenceGraph mirProgram
        putStrLn "Interference Graph:"
        mapM_ print (Map.toList interferenceGraph)
      Left err -> error err
  ShowX86 fileName -> do
    processedAst <- processToMir fileName
    case processedAst of
      Right mirProgram -> do
        let allocationResult = Allocation.allocateProgram mirProgram
        let x86Program = X86.Translate.translateProgram allocationResult
        putStrLn x86Program
      Left err -> error err
  X86ToFile fileName outFile -> do
    processedAst <- processToMir fileName
    case processedAst of
      Right mirProgram -> do
        let optMirProgram = Mir.CopyPropagation.optimizeProgram mirProgram
        let allocationResult = Allocation.allocateProgram optMirProgram
        let x86Program = X86.Translate.translateProgram allocationResult
        writeFile outFile x86Program
      Left err -> error err

-- Helper function to reduce repetition
processToMir :: String -> IO (Either String Mir.Types.Program)
processToMir fileName = do
  astResult <- parseFile fileName
  case astResult of
    Right ast -> do
      typedResult <- typeCheckAst ast
      case typedResult of
        Right typedAst -> return $ Right (Mir.Translate.transProgram typedAst)
        Left err -> return $ Left err
    Left err -> return $ Left err