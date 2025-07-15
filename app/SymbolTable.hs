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
    allocateStackSlot,
    allocateTempRegister,
    getStackOffset,
    getTempRegister,
    getStaticOffset,
    allocateStaticSlot,
    resetFrameAllocation,
    dataList,
  )
where

import Ast.Types (VarDef (..))
import Data.Map qualified as Map
import TypeSystem (Id, Ty (..), sizeOf)

type BlockId = Int

type FunctionId = String

data SymbolStorage
  = Argument
  | Auto
  | Static
  | Extern
  deriving (Eq)

instance Show SymbolStorage where
  show Argument = "Argument"
  show Auto = "Auto"
  show Static = "Static"
  show Extern = "Extern"

data Symbol = Symbol
  { symbolId :: Id,
    symbolTy :: Ty,
    symbolStorage :: SymbolStorage,
    addressTaken :: Bool,
    stackOffset :: Maybe Int,
    tempRegister :: Maybe Int,
    staticOffset :: Maybe Int
  }
  deriving (Eq)

instance Show Symbol where
  show Symbol {symbolTy, symbolStorage} =
    "Symbol { ty: " ++ show symbolTy ++ ", alloc: " ++ show symbolStorage ++ " }"

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
    nextBlockId :: BlockId,
    dataEnv :: Environment,
    currentArgOffset :: Int,
    currentLocalOffset :: Int,
    nextTempId :: Int,
    staticDataSize :: Int
  }
  deriving (Eq)

instance Show SymbolTable where
  show SymbolTable {blockEnvs, dataEnv, currentArgOffset, currentLocalOffset, nextTempId, staticDataSize} =
    "SymbolTable { blockEnvs: "
      ++ show (Map.toList blockEnvs)
      ++ ", dataEnv: "
      ++ show dataEnv
      ++ ", currentArgOffset: "
      ++ show currentArgOffset
      ++ ", currentLocalOffset: "
      ++ show currentLocalOffset
      ++ ", nextTempId: "
      ++ show nextTempId
      ++ ", staticDataSize: "
      ++ show staticDataSize
      ++ " }"

globalSymbolTable :: SymbolTable
globalSymbolTable =
  SymbolTable
    { blockEnvs = Map.singleton 0 globalEnv,
      nextBlockId = 1,
      dataEnv = globalEnv,
      currentArgOffset = 16,
      currentLocalOffset = 0,
      nextTempId = 0,
      staticDataSize = 0
    }

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
                symbolStorage = Static,
                addressTaken = False,
                stackOffset = Nothing,
                tempRegister = Nothing,
                staticOffset = Nothing
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
                symbolStorage = alloc,
                addressTaken = False,
                stackOffset = Nothing,
                tempRegister = Nothing,
                staticOffset = Nothing
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

dataList :: SymbolTable -> [(Id, Symbol)]
dataList SymbolTable {dataEnv} =
  Map.toList (envSymbols dataEnv)

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

-- Stack frame management functions
allocateStackSlot :: Id -> BlockId -> SymbolStorage -> SymbolTable -> SymbolTable
allocateStackSlot
  identifier
  blockId
  storage
  st@SymbolTable
    { blockEnvs = blockEnvs',
      currentArgOffset,
      currentLocalOffset
    } =
    case Map.lookup blockId blockEnvs' of
      Just env ->
        case Map.lookup identifier (envSymbols env) of
          Just symbol ->
            let (offset, newSt) = case storage of
                  Argument ->
                    let size = sizeOf (symbolTy symbol)
                        newOffset = currentArgOffset + size
                     in (currentArgOffset, st {currentArgOffset = newOffset})
                  Auto ->
                    let size = sizeOf (symbolTy symbol)
                        newOffset = currentLocalOffset - size
                     in (newOffset, st {currentLocalOffset = newOffset})
                  _ -> (0, st)
                updatedSymbol = symbol {stackOffset = Just offset}
                updatedSymbols = Map.insert identifier updatedSymbol (envSymbols env)
                updatedEnv = env {envSymbols = updatedSymbols}
                updatedBlockEnvs = Map.insert blockId updatedEnv (blockEnvs newSt)
             in newSt {blockEnvs = updatedBlockEnvs}
          Nothing -> error $ "Symbol with ID " ++ show identifier ++ " not found in block " ++ show blockId
      Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

allocateTempRegister :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateTempRegister identifier blockId st@SymbolTable {blockEnvs, nextTempId} =
  case Map.lookup blockId blockEnvs of
    Just env ->
      case Map.lookup identifier (envSymbols env) of
        Just symbol ->
          let updatedSymbol = symbol {tempRegister = Just nextTempId}
              updatedSymbols = Map.insert identifier updatedSymbol (envSymbols env)
              updatedEnv = env {envSymbols = updatedSymbols}
              updatedBlockEnvs = Map.insert blockId updatedEnv blockEnvs
           in st {blockEnvs = updatedBlockEnvs, nextTempId = nextTempId + 1}
        Nothing -> error $ "Symbol with ID " ++ show identifier ++ " not found in block " ++ show blockId
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

allocateStaticSlot :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateStaticSlot identifier blockId st@SymbolTable {blockEnvs, staticDataSize, dataEnv} =
  case Map.lookup blockId blockEnvs of
    Just env ->
      case Map.lookup identifier (envSymbols env) of
        Just symbol ->
          let size = sizeOf (symbolTy symbol)
              updatedSymbol = symbol {staticOffset = Just staticDataSize}
              updatedSymbols = Map.insert identifier updatedSymbol (envSymbols env)
              updatedEnv = env {envSymbols = updatedSymbols}
              updatedBlockEnvs = Map.insert blockId updatedEnv blockEnvs
              newDataEnvMap = Map.insert identifier updatedSymbol (envSymbols dataEnv)
              newDataEnv = dataEnv {envSymbols = newDataEnvMap}
           in st {blockEnvs = updatedBlockEnvs, staticDataSize = staticDataSize + size, dataEnv = newDataEnv}
        Nothing -> error $ "Symbol with ID " ++ show identifier ++ " not found in block " ++ show blockId
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

getStackOffset :: Id -> BlockId -> SymbolTable -> Maybe Int
getStackOffset identifier blockId st =
  stackOffset =<< lookupSymbol identifier blockId st

getTempRegister :: Id -> BlockId -> SymbolTable -> Maybe Int
getTempRegister identifier blockId st =
  tempRegister =<< lookupSymbol identifier blockId st

getStaticOffset :: Id -> BlockId -> SymbolTable -> Maybe Int
getStaticOffset identifier blockId st =
  staticOffset =<< lookupSymbol identifier blockId st

resetFrameAllocation :: SymbolTable -> SymbolTable
resetFrameAllocation st =
  st
    { currentArgOffset = 16,
      currentLocalOffset = 0,
      nextTempId = 0
    }