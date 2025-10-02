{-# LANGUAGE ApplicativeDo #-}

module CLI.Commands
  ( Command (..),
    Options (..),
    commandParser,
    opts,
  )
where

import Options.Applicative

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
