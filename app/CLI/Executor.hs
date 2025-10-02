module CLI.Executor
  ( executeCommand,
  )
where

import CLI.Commands
import CompilerPipeline
import Data.Map qualified as Map
import Mir.Allocation qualified as Allocation
import Mir.Interference qualified as Interference
import Mir.Liveness qualified as Liveness
import Pipeline

-- | Execute a CLI command
executeCommand :: Command -> IO ()
executeCommand cmd = case cmd of
  ShowAst fileName -> do
    ast <- runPipelineWithError astPipeline fileName
    print ast
  ShowSemant fileName -> do
    (typedAst, symbolTable) <- runPipelineWithError semantPipeline fileName
    putStrLn "Type checking successful. Typed AST:"
    print typedAst
    putStrLn "Symbol Table:"
    print symbolTable
  ShowMir fileName -> do
    (mirProgram, symbolTable) <- runPipelineWithError mirPipeline fileName
    putStrLn "MIR Program:"
    print mirProgram
    putStrLn "Symbol Table:"
    print symbolTable
  ShowMirOptimized fileName -> do
    optimizedProgram <- runPipelineWithError (extractFirst optimizedMirPipeline) fileName
    print optimizedProgram
  ShowX86 fileName -> do
    x86Program <- runPipelineWithError x86Pipeline fileName
    putStrLn x86Program
  X86ToFile fileName outFile -> do
    x86Program <- runPipelineWithError x86Pipeline fileName
    writeFile outFile x86Program
  ShowMirLive fileName -> do
    mirProgram <- runPipelineWithError (extractFirst mirPipeline) fileName
    let livenessInfo = Liveness.analyzeProgramLiveness mirProgram
    print mirProgram
    putStrLn "Liveness Information:"
    print livenessInfo
  ShowMirColor fileName -> do
    (mirProgram, _symbolTable) <- runPipelineWithError mirPipeline fileName
    let allocationResult = Allocation.allocateProgram mirProgram
    let livenessInfo = Liveness.analyzeProgramLiveness mirProgram
    print mirProgram
    putStrLn "Liveness Information:"
    print livenessInfo
    putStrLn "Allocation Result:"
    print allocationResult
  MirToFile fileName outFile -> do
    (mirProgram, _symbolTable) <- runPipelineWithError mirPipeline fileName
    writeFile outFile (show mirProgram)
  ShowMirInterference fileName -> do
    (mirProgram, _symbolTable) <- runPipelineWithError mirPipeline fileName
    print mirProgram
    let interferenceGraph = Interference.programInterferenceGraph mirProgram
    putStrLn "Interference Graph:"
    mapM_ print (Map.toList interferenceGraph)
