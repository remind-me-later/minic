{-# LANGUAGE TemplateHaskell #-}

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
    -- Lenses
    symbolTy,
  )
where

import Ast.Types (VarDef (..))
import Control.Lens
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
    _symbolTy :: Ty,
    symbolStorage :: SymbolStorage,
    addressTaken :: Bool,
    stackOffset :: Maybe Int,
    tempRegister :: Maybe Int,
    staticOffset :: Maybe Int
  }
  deriving (Eq)

makeLenses ''Symbol

instance Show Symbol where
  show Symbol {_symbolTy, symbolStorage} =
    "Symbol { ty: " ++ show _symbolTy ++ ", alloc: " ++ show symbolStorage ++ " }"

data Environment = Env
  { _envBlockId :: BlockId,
    _envSymbols :: Map.Map Id Symbol,
    _parentBlockId :: Maybe BlockId
  }
  deriving (Eq)

makeLenses ''Environment

instance Show Environment where
  show Env {_envBlockId, _envSymbols} =
    "Env { blockId: "
      ++ show _envBlockId
      ++ ", symbols: "
      ++ show (Map.toList _envSymbols)
      ++ " }"

globalEnv :: Environment
globalEnv = Env {_envBlockId = 0, _envSymbols = Map.empty, _parentBlockId = Nothing}

emptyEnv :: BlockId -> BlockId -> Environment
emptyEnv _envBlockId parentBlockId = Env {_envBlockId, _envSymbols = Map.empty, _parentBlockId = Just parentBlockId}

data SymbolTable = SymbolTable
  { _blockEnvs :: Map.Map BlockId Environment,
    nextBlockId :: BlockId,
    dataEnv :: Environment,
    currentArgOffset :: Int,
    currentLocalOffset :: Int,
    nextTempId :: Int,
    staticDataSize :: Int
  }
  deriving (Eq)

makeLenses ''SymbolTable

instance Show SymbolTable where
  show SymbolTable {_blockEnvs, dataEnv, currentArgOffset, currentLocalOffset, nextTempId, staticDataSize} =
    "SymbolTable { blockEnvs: "
      ++ show (Map.toList _blockEnvs)
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

globalSymbolTable ::
  SymbolTable
globalSymbolTable =
  SymbolTable
    { _blockEnvs = Map.singleton 0 globalEnv,
      nextBlockId = 1,
      dataEnv = globalEnv,
      currentArgOffset = 16,
      currentLocalOffset = 0,
      nextTempId = 0,
      staticDataSize = 0
    }

insertBlock :: Environment -> SymbolTable -> (BlockId, SymbolTable)
insertBlock env st@SymbolTable {_blockEnvs, nextBlockId} =
  let newId = nextBlockId
      newSt =
        st
          { _blockEnvs = Map.insert newId env _blockEnvs,
            nextBlockId = nextBlockId + 1
          }
   in (newId, newSt)

lookupBlock :: BlockId -> SymbolTable -> Maybe Environment
lookupBlock blockId SymbolTable {_blockEnvs} = Map.lookup blockId _blockEnvs

currentBlockId :: SymbolTable -> BlockId
currentBlockId SymbolTable {nextBlockId} = nextBlockId - 1

insertFunToEnv :: BlockId -> FunctionId -> Ty -> SymbolTable -> SymbolTable
insertFunToEnv blockId funId funTy st@SymbolTable {_blockEnvs} =
  case Map.lookup blockId _blockEnvs of
    Just env ->
      let newSymbol =
            Symbol
              { symbolId = funId,
                _symbolTy = funTy,
                symbolStorage = Static,
                addressTaken = False,
                stackOffset = Nothing,
                tempRegister = Nothing,
                staticOffset = Nothing
              }
          newEnv = env {_envSymbols = Map.insert funId newSymbol (_envSymbols env)}
          newBlockEnvs = Map.insert blockId newEnv _blockEnvs
       in st {_blockEnvs = newBlockEnvs}
    Nothing -> st

lookupSymbol :: Id -> BlockId -> SymbolTable -> Maybe Symbol
lookupSymbol identifier blockId st@SymbolTable {_blockEnvs} =
  case Map.lookup blockId _blockEnvs of
    Just env ->
      case Map.lookup identifier (_envSymbols env) of
        Just symbol -> Just symbol
        Nothing -> case _parentBlockId env of
          Just parentId -> lookupSymbol identifier parentId st
          Nothing -> Nothing
    Nothing -> Nothing

insertVar :: VarDef -> SymbolStorage -> BlockId -> SymbolTable -> SymbolTable
insertVar varDef alloc blockId st@SymbolTable {_blockEnvs} =
  case Map.lookup blockId _blockEnvs of
    Just env ->
      let newSymbol =
            Symbol
              { symbolId = _varDefId varDef,
                _symbolTy = _varDefTy varDef,
                symbolStorage = alloc,
                addressTaken = False,
                stackOffset = Nothing,
                tempRegister = Nothing,
                staticOffset = Nothing
              }
          newEnv = env {_envSymbols = Map.insert (_varDefId varDef) newSymbol (_envSymbols env)}
          newBlockEnvs = Map.insert blockId newEnv _blockEnvs
       in st {_blockEnvs = newBlockEnvs}
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

insertArg :: VarDef -> BlockId -> SymbolTable -> SymbolTable
insertArg varDef blockId st@SymbolTable {_blockEnvs} =
  insertVar varDef Argument blockId st {_blockEnvs}

openEnv :: SymbolTable -> (SymbolTable, BlockId)
openEnv st@SymbolTable {_blockEnvs} =
  let newId = nextBlockId st
      env = emptyEnv newId (currentBlockId st)
      newBlockEnvs = Map.insert newId env _blockEnvs
   in (st {_blockEnvs = newBlockEnvs, nextBlockId = newId + 1}, newId)

prevEnv :: BlockId -> SymbolTable -> BlockId
prevEnv blockId SymbolTable {_blockEnvs} =
  case Map.lookup blockId _blockEnvs of
    Just env -> case _parentBlockId env of
      Just parentId -> parentId
      Nothing -> error $ "No parent block for block ID " ++ show blockId
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

peekEnv :: BlockId -> SymbolTable -> Environment
peekEnv blockId SymbolTable {_blockEnvs} =
  case Map.lookup blockId _blockEnvs of
    Just env -> env
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

toList :: BlockId -> SymbolTable -> [Symbol]
toList blockId SymbolTable {_blockEnvs} =
  case Map.lookup blockId _blockEnvs of
    Just env -> Map.elems (_envSymbols env)
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

dataList :: SymbolTable -> [(Id, Symbol)]
dataList SymbolTable {dataEnv} =
  Map.toList (_envSymbols dataEnv)

setAddressTaken :: Id -> BlockId -> SymbolTable -> SymbolTable
setAddressTaken identifier blockId st@SymbolTable {_blockEnvs} =
  case Map.lookup blockId _blockEnvs of
    Just env ->
      case Map.lookup identifier (_envSymbols env) of
        Just symbol ->
          let updatedSymbol = symbol {addressTaken = True}
              updatedSymbols = Map.insert identifier updatedSymbol (_envSymbols env)
              updatedEnv = env {_envSymbols = updatedSymbols}
              updatedBlockEnvs = Map.insert blockId updatedEnv _blockEnvs
           in st {_blockEnvs = updatedBlockEnvs}
        Nothing -> error $ "Symbol with ID " ++ show identifier ++ " not found in block " ++ show blockId
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

-- Stack frame management functions
allocateStackSlot :: Id -> BlockId -> SymbolStorage -> SymbolTable -> SymbolTable
allocateStackSlot
  identifier
  blockId
  storage
  st@SymbolTable
    { _blockEnvs = blockEnvs',
      currentArgOffset,
      currentLocalOffset
    } =
    case Map.lookup blockId blockEnvs' of
      Just env ->
        case Map.lookup identifier (_envSymbols env) of
          Just symbol ->
            let (offset, newSt) = case storage of
                  Argument ->
                    let size = sizeOf (_symbolTy symbol)
                        newOffset = currentArgOffset + size
                     in (currentArgOffset, st {currentArgOffset = newOffset})
                  Auto ->
                    let size = sizeOf (_symbolTy symbol)
                        newOffset = currentLocalOffset - size
                     in (newOffset, st {currentLocalOffset = newOffset})
                  _ -> (0, st)
                updatedSymbol = symbol {stackOffset = Just offset}
                updatedSymbols = Map.insert identifier updatedSymbol (_envSymbols env)
                updatedEnv = env {_envSymbols = updatedSymbols}
                updatedBlockEnvs = Map.insert blockId updatedEnv (_blockEnvs newSt)
             in newSt {_blockEnvs = updatedBlockEnvs}
          Nothing -> error $ "Symbol with ID " ++ show identifier ++ " not found in block " ++ show blockId
      Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

allocateTempRegister :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateTempRegister identifier blockId st@SymbolTable {_blockEnvs, nextTempId} =
  case Map.lookup blockId _blockEnvs of
    Just env ->
      case Map.lookup identifier (_envSymbols env) of
        Just symbol ->
          let updatedSymbol = symbol {tempRegister = Just nextTempId}
              updatedSymbols = Map.insert identifier updatedSymbol (_envSymbols env)
              updatedEnv = env {_envSymbols = updatedSymbols}
              updatedBlockEnvs = Map.insert blockId updatedEnv _blockEnvs
           in st {_blockEnvs = updatedBlockEnvs, nextTempId = nextTempId + 1}
        Nothing -> error $ "Symbol with ID " ++ show identifier ++ " not found in block " ++ show blockId
    Nothing -> error $ "Block with ID " ++ show blockId ++ " not found in symbol table."

allocateStaticSlot :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateStaticSlot identifier blockId st@SymbolTable {_blockEnvs, staticDataSize, dataEnv} =
  case Map.lookup blockId _blockEnvs of
    Just env ->
      case Map.lookup identifier (_envSymbols env) of
        Just symbol ->
          let size = sizeOf (_symbolTy symbol)
              updatedSymbol = symbol {staticOffset = Just staticDataSize}
              updatedSymbols = Map.insert identifier updatedSymbol (_envSymbols env)
              updatedEnv = env {_envSymbols = updatedSymbols}
              updatedBlockEnvs = Map.insert blockId updatedEnv _blockEnvs
              newDataEnvMap = Map.insert identifier updatedSymbol (_envSymbols dataEnv)
              newDataEnv = dataEnv {_envSymbols = newDataEnvMap}
           in st {_blockEnvs = updatedBlockEnvs, staticDataSize = staticDataSize + size, dataEnv = newDataEnv}
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

blockEnv :: BlockId -> Lens' SymbolTable (Maybe Environment)
blockEnv blockId = blockEnvs . at blockId

envSymbol :: Id -> Lens' Environment (Maybe Symbol)
envSymbol symbolId = envSymbols . at symbolId

-- Traversal for symbols in a specific block
blockSymbols :: BlockId -> Traversal' SymbolTable Symbol
blockSymbols blockId = blockEnvs . ix blockId . envSymbols . traverse