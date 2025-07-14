module SymbolTable
  ( BlockId,
    FunctionId,
    SymbolStorage (..),
    Symbol (..),
    Environment (..),
    globalEnv,
    emptyEnv,
    SymbolTable (..),
    globalSymbolTable,
    insertBlock,
    lookupBlock,
    currentBlockId,
    insertFunToEnv,
    lookupSymbol,
    insertVar,
    insertArg,
    openEnv,
    prevEnv,
    peekEnv,
    toList,
    setAddressTaken,
  )
where

import Ast.Types (VarDef (..))
import Data.Map qualified as Map
import TypeSystem (Id, Ty (..))

type BlockId = Int

type FunctionId = String

data SymbolStorage
  = Argument
  | Local
  | Global
  deriving (Eq)

instance Show SymbolStorage where
  show Argument = "Argument"
  show Local = "Local"
  show Global = "Global"

data Symbol = Symbol
  { symbolId :: Id,
    symbolTy :: Ty,
    symbolAlloc :: SymbolStorage,
    addressTaken :: Bool
  }
  deriving (Eq)

instance Show Symbol where
  show Symbol {symbolTy, symbolAlloc} =
    "Symbol { ty: " ++ show symbolTy ++ ", alloc: " ++ show symbolAlloc ++ " }"

data Environment = Env
  { envBlockId :: BlockId,
    envSymbols :: Map.Map Id Symbol,
    parentBlockId :: Maybe BlockId
  }
  deriving (Eq)

instance Show Environment where
  show Env {envBlockId, envSymbols} =
    "Env { blockId: "
      ++ show envBlockId
      ++ ", symbols: "
      ++ show (Map.toList envSymbols)
      ++ " }"

globalEnv :: Environment
globalEnv = Env {envBlockId = 0, envSymbols = Map.empty, parentBlockId = Nothing}

emptyEnv :: BlockId -> BlockId -> Environment
emptyEnv envBlockId parentBlockId = Env {envBlockId, envSymbols = Map.empty, parentBlockId = Just parentBlockId}

data SymbolTable = SymbolTable
  { blockEnvs :: Map.Map BlockId Environment,
    nextBlockId :: BlockId
  }
  deriving (Show, Eq)

globalSymbolTable :: SymbolTable
globalSymbolTable = SymbolTable {blockEnvs = Map.singleton 0 globalEnv, nextBlockId = 1}

insertBlock :: Environment -> SymbolTable -> (BlockId, SymbolTable)
insertBlock env st@SymbolTable {blockEnvs, nextBlockId} =
  let newId = nextBlockId
      newSt =
        st
          { blockEnvs = Map.insert newId env blockEnvs,
            nextBlockId = nextBlockId + 1
          }
   in (newId, newSt)

lookupBlock :: BlockId -> SymbolTable -> Maybe Environment
lookupBlock blockId SymbolTable {blockEnvs} = Map.lookup blockId blockEnvs

currentBlockId :: SymbolTable -> BlockId
currentBlockId SymbolTable {nextBlockId} = nextBlockId - 1

insertFunToEnv :: BlockId -> FunctionId -> Ty -> SymbolTable -> SymbolTable
insertFunToEnv blockId funId funTy st@SymbolTable {blockEnvs} =
  case Map.lookup blockId blockEnvs of
    Just env ->
      let newSymbol =
            Symbol
              { symbolId = funId,
                symbolTy = funTy,
                symbolAlloc = Global,
                addressTaken = False
              }
          newEnv = env {envSymbols = Map.insert funId newSymbol (envSymbols env)}
          newBlockEnvs = Map.insert blockId newEnv blockEnvs
       in st {blockEnvs = newBlockEnvs}
    Nothing -> st

lookupSymbol :: Id -> BlockId -> SymbolTable -> Maybe Symbol
lookupSymbol identifier blockId st@SymbolTable {blockEnvs} =
  case Map.lookup blockId blockEnvs of
    Just env ->
      case Map.lookup identifier (envSymbols env) of
        Just symbol -> Just symbol
        Nothing -> case parentBlockId env of
          Just parentId -> lookupSymbol identifier parentId st
          Nothing -> Nothing
    Nothing -> Nothing

insertVar :: VarDef -> SymbolStorage -> BlockId -> SymbolTable -> SymbolTable
insertVar varDef alloc blockId st@SymbolTable {blockEnvs} =
  case Map.lookup blockId blockEnvs of
    Just env ->
      let newSymbol =
            Symbol
              { symbolId = varDefId varDef,
                symbolTy = varDefTy varDef,
                symbolAlloc = alloc,
                addressTaken = False
              }
          newEnv = env {envSymbols = Map.insert (varDefId varDef) newSymbol (envSymbols env)}
          newBlockEnvs = Map.insert blockId newEnv blockEnvs
       in st {blockEnvs = newBlockEnvs}
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

insertArg :: VarDef -> BlockId -> SymbolTable -> SymbolTable
insertArg varDef blockId st@SymbolTable {blockEnvs} =
  insertVar varDef Argument blockId st {blockEnvs}

openEnv :: SymbolTable -> (SymbolTable, BlockId)
openEnv st@SymbolTable {blockEnvs} =
  let newId = nextBlockId st
      env = emptyEnv newId (currentBlockId st)
      newBlockEnvs = Map.insert newId env blockEnvs
   in (st {blockEnvs = newBlockEnvs, nextBlockId = newId + 1}, newId)

prevEnv :: BlockId -> SymbolTable -> BlockId
prevEnv blockId SymbolTable {blockEnvs} =
  case Map.lookup blockId blockEnvs of
    Just env -> case parentBlockId env of
      Just parentId -> parentId
      Nothing -> error $ "No parent block for block ID " ++ show blockId
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

peekEnv :: BlockId -> SymbolTable -> Environment
peekEnv blockId SymbolTable {blockEnvs} =
  case Map.lookup blockId blockEnvs of
    Just env -> env
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

toList :: BlockId -> SymbolTable -> [Symbol]
toList blockId SymbolTable {blockEnvs} =
  case Map.lookup blockId blockEnvs of
    Just env -> Map.elems (envSymbols env)
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

setAddressTaken :: Id -> BlockId -> SymbolTable -> SymbolTable
setAddressTaken identifier blockId st@SymbolTable {blockEnvs} =
  case Map.lookup blockId blockEnvs of
    Just env ->
      case Map.lookup identifier (envSymbols env) of
        Just symbol ->
          let updatedSymbol = symbol {addressTaken = True}
              updatedSymbols = Map.insert identifier updatedSymbol (envSymbols env)
              updatedEnv = env {envSymbols = updatedSymbols}
              updatedBlockEnvs = Map.insert blockId updatedEnv blockEnvs
           in st {blockEnvs = updatedBlockEnvs}
        Nothing -> error $ "Symbol with ID " ++ show identifier ++ " not found in block " ++ show blockId
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."