module Ast.Semant
  ( typeProgram,
    TypedProgram,
    TypedFun,
    TypedBlock,
    TypedStmt,
    TypedExp,
  )
where

import Ast.Semant.State
import Ast.Semant.Statements
import Ast.Types
import Control.Lens
import Control.Monad.State (State, runState)
import SymbolTable (SymbolTable)
import SymbolTable qualified
import TypeSystem

-- | Type check a function
typeFun :: RawFun -> State TypingState TypedFun
typeFun Fun {_funId, _funArgs, _funRetTy, _funBody = Block {_blockStmts, _blockId}} = do
  parentBlockId' <- use currentBlockId
  openEnvInState _blockId parentBlockId'
  -- Reset frame allocation for this function
  symbolTable %= SymbolTable.resetFrameAllocation
  mapM_ insertArgInState _funArgs
  setCurrentFunInState _funId
  annotatedStmts <- mapM typeStmt _blockStmts
  closeEnvInState
  return Fun {_funId, _funArgs, _funRetTy, _funBody = Block {_blockId, _blockStmts = annotatedStmts}}

-- | No typing is performed for external functions since their types are assumed to be correct.
typeExternFun :: ExternFun -> State TypingState ExternFun
typeExternFun = return

-- | Type check a program
typeProgram' :: RawProgram -> State TypingState TypedProgram
typeProgram' Program {programFuncs, programExternFuns, programMainFun} = do
  programFuncs' <- mapM typeFun programFuncs
  programExternFuns' <- mapM typeExternFun programExternFuns
  programMainFun' <- case programMainFun of
    Just f -> do
      typedMainFun <- typeFun f
      return (Just typedMainFun)
    Nothing -> do
      return Nothing

  return
    Program
      { programFuncs = programFuncs',
        programExternFuns = programExternFuns',
        programMainFun = programMainFun'
      }

-- | Build the global symbol table from a program
buildGlobalSymbolTable :: RawProgram -> SymbolTable
buildGlobalSymbolTable Program {programFuncs, programExternFuns} =
  insertFunctions programFuncs $
    insertExternFunctions programExternFuns globalSymbolTable'
  where
    globalSymbolTable' = SymbolTable.globalSymbolTable
    blockId' = 0

    insertFunctions :: [RawFun] -> SymbolTable -> SymbolTable
    insertFunctions [] symbolTable' = symbolTable'
    insertFunctions (f : fs) symbolTable' =
      let funTy =
            FunTy
              { funTyArgs = map _varDefTy (_funArgs f),
                funTyRetTy = _funRetTy f
              }
          _funId' = _funId f
       in insertFunctions fs (SymbolTable.insertFunToEnv blockId' _funId' funTy symbolTable')

    insertExternFunctions :: [RawExternFun] -> SymbolTable -> SymbolTable
    insertExternFunctions [] symbolTable' = symbolTable'
    insertExternFunctions (ef : efs) symbolTable' =
      let efTy =
            FunTy
              { funTyArgs = externFunArgs ef,
                funTyRetTy = externFunRetTy ef
              }
          efId = externFunId ef
       in insertExternFunctions efs (SymbolTable.insertExternFunToEnv blockId' efId efTy symbolTable')

-- | Type check a program and return typed AST with symbol table or errors
typeProgram :: RawProgram -> Either [String] (TypedProgram, SymbolTable)
typeProgram program =
  let initialState =
        TypingState
          { _symbolTable = buildGlobalSymbolTable program,
            _errors = [],
            _curFun = Nothing,
            _currentBlockId = 0
          }

      (typedProgram, finalState) = runState (typeProgram' program) initialState
   in if null (_errors finalState)
        then Right (typedProgram, _symbolTable finalState)
        else Left (_errors finalState)
