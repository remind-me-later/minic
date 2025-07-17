module CompilerPipeline where

import Ast.Parse qualified
import Ast.Semant qualified
import Ast.Types qualified
import Data.Bifunctor qualified as Bifunctor
import Mir.Allocation qualified as Allocation
import Mir.CopyPropagation qualified as Mir
import Mir.Translate qualified
import Mir.Types qualified
import Pipeline
import SymbolTable qualified
import X86.Translate qualified

-- Individual pipeline steps
parseStep :: Pipeline String Ast.Types.RawProgram
parseStep = liftPipeline $ \fileName -> do
  contents <- readFile fileName
  parseResult <- Ast.Parse.program fileName contents
  return $ case parseResult of
    Right ast -> Right ast
    Left err -> Left $ "Parsing failed: " ++ show err

typeCheckStep :: Pipeline Ast.Types.RawProgram (Ast.Semant.TypedProgram, SymbolTable.SymbolTable)
typeCheckStep = purePipeline $ \ast ->
  case Ast.Semant.typeProgram ast of
    Right result -> result
    Left errs -> error $ "Type checking failed: " ++ show errs

mirTranslationStep :: Pipeline (Ast.Semant.TypedProgram, SymbolTable.SymbolTable) (Mir.Types.Program, SymbolTable.SymbolTable)
mirTranslationStep = purePipeline $ uncurry Mir.Translate.transProgram

optimizationStep :: Pipeline (Mir.Types.Program, SymbolTable.SymbolTable) (Mir.Types.Program, SymbolTable.SymbolTable)
optimizationStep = purePipeline $ Bifunctor.first Mir.optimizeProgram

allocationStep :: Pipeline (Mir.Types.Program, SymbolTable.SymbolTable) (Mir.Types.Program, SymbolTable.SymbolTable)
allocationStep = purePipeline $ Bifunctor.first Allocation.allocateProgram

x86TranslationStep :: Pipeline (Mir.Types.Program, SymbolTable.SymbolTable) String
x86TranslationStep = purePipeline $ uncurry X86.Translate.translateProgram

-- Complete pipelines
astPipeline :: Pipeline String Ast.Types.RawProgram
astPipeline = parseStep

semantPipeline :: Pipeline String (Ast.Semant.TypedProgram, SymbolTable.SymbolTable)
semantPipeline = parseStep >>> typeCheckStep

mirPipeline :: Pipeline String (Mir.Types.Program, SymbolTable.SymbolTable)
mirPipeline = parseStep >>> typeCheckStep >>> mirTranslationStep

optimizedMirPipeline :: Pipeline String (Mir.Types.Program, SymbolTable.SymbolTable)
optimizedMirPipeline = mirPipeline >>> optimizationStep

x86Pipeline :: Pipeline String String
x86Pipeline = optimizedMirPipeline >>> allocationStep >>> x86TranslationStep

-- Convenience function for running pipelines that need to return tuples
extractFirst :: Pipeline a (b, c) -> Pipeline a b
extractFirst pipeline = Pipeline $ \a -> do
  result <- runPipeline pipeline a
  return $ fmap fst result

extractSecond :: Pipeline a (b, c) -> Pipeline a c
extractSecond pipeline = Pipeline $ \a -> do
  result <- runPipeline pipeline a
  return $ fmap snd result