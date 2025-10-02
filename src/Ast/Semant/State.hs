{-# LANGUAGE TemplateHaskell #-}

module Ast.Semant.State
  ( TypingState (..),
    symbolTable,
    currentBlockId,
    errors,
    curFun,
    addErrorInState,
    insertVarInState,
    insertArgInState,
    openEnvInState,
    closeEnvInState,
    lookupSymbolInState,
    currentFunInState,
    setCurrentFunInState,
  )
where

import Ast.Types
import Control.Lens
import Control.Monad.State (State)
import SymbolTable qualified
import SymbolTable.Types (BlockId, Symbol (..), SymbolTable, VarSymbolStorage (..))
import TypeSystem

-- | State for type checking
data TypingState = TypingState
  { _symbolTable :: SymbolTable,
    _currentBlockId :: BlockId,
    _errors :: [String],
    _curFun :: Maybe Id
  }

makeLenses ''TypingState

-- | Add an error message to the typing state
addErrorInState :: String -> State TypingState ()
addErrorInState err = errors %= (err :)

-- | Insert a variable into the current scope
insertVarInState :: VarDef -> VarSymbolStorage -> State TypingState ()
insertVarInState varDef varSymbolStorage' = do
  curBlockId <- use currentBlockId
  symbolTable %= SymbolTable.insertVar varDef varSymbolStorage' curBlockId

-- | Insert a function argument into the current scope
insertArgInState :: VarDef -> State TypingState ()
insertArgInState varDef = do
  curBlockId <- use currentBlockId
  symbolTable %= SymbolTable.insertArg varDef curBlockId

-- | Open a new environment (scope)
openEnvInState :: BlockId -> BlockId -> State TypingState ()
openEnvInState blockId' parentBlockId' = do
  st <- use symbolTable
  symbolTable .= SymbolTable.openEnv blockId' parentBlockId' st
  currentBlockId .= blockId'

-- | Close the current environment (scope)
closeEnvInState :: State TypingState ()
closeEnvInState = do
  st <- use symbolTable
  curBlock <- use currentBlockId
  currentBlockId .= SymbolTable.prevEnv curBlock st

-- | Look up a symbol in the current scope
lookupSymbolInState :: Id -> State TypingState (Maybe Symbol)
lookupSymbolInState identifier = do
  st <- use symbolTable
  curBlockId <- use currentBlockId
  return $ SymbolTable.lookupSymbol identifier curBlockId st

-- | Get the current function symbol
currentFunInState :: State TypingState (Maybe Symbol)
currentFunInState = do
  maybeFunId <- use curFun
  symbolTable' <- use symbolTable
  blockId' <- use currentBlockId
  return $ case maybeFunId of
    Nothing -> Nothing
    Just _funId -> SymbolTable.lookupSymbol _funId blockId' symbolTable'

-- | Set the current function being type-checked
setCurrentFunInState :: Id -> State TypingState ()
setCurrentFunInState = assign curFun . Just
