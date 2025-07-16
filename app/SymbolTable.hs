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

import Ast.Lenses
import Ast.Types (VarDef (..))
import Control.Applicative ((<|>))
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
  { _symbolId :: Id,
    _symbolTy :: Ty,
    _symbolStorage :: SymbolStorage,
    _addressTaken :: Bool,
    _stackOffset :: Maybe Int,
    _tempRegister :: Maybe Int,
    _staticOffset :: Maybe Int
  }
  deriving (Eq)

makeLenses ''Symbol

instance Show Symbol where
  show Symbol {_symbolTy, _symbolStorage} =
    "Symbol { ty: " ++ show _symbolTy ++ ", alloc: " ++ show _symbolStorage ++ " }"

data Environment = Env
  { _envBlockId :: BlockId,
    _envSymbolMap :: Map.Map Id Symbol,
    _parentBlockId :: Maybe BlockId
  }
  deriving (Eq)

makeLenses ''Environment

instance Show Environment where
  show Env {_envBlockId, _envSymbolMap} =
    "Env { blockId: "
      ++ show _envBlockId
      ++ ", symbols: "
      ++ show (Map.toList _envSymbolMap)
      ++ " }"

globalEnv :: Environment
globalEnv = Env {_envBlockId = 0, _envSymbolMap = Map.empty, _parentBlockId = Nothing}

emptyEnv :: BlockId -> BlockId -> Environment
emptyEnv _envBlockId parentBlockId' = Env {_envBlockId, _envSymbolMap = Map.empty, _parentBlockId = Just parentBlockId'}

data SymbolTable = SymbolTable
  { _blockEnvs :: Map.Map BlockId Environment,
    _nextBlockId :: BlockId,
    _dataEnv :: Environment,
    _currentArgOffset :: Int,
    _currentLocalOffset :: Int,
    _nextTempId :: Int,
    _staticDataSize :: Int
  }
  deriving (Eq)

makeLenses ''SymbolTable

instance Show SymbolTable where
  show SymbolTable {_blockEnvs, _dataEnv, _currentArgOffset, _currentLocalOffset, _nextTempId, _staticDataSize} =
    "SymbolTable { blockEnvs: "
      ++ show (Map.toList _blockEnvs)
      ++ ", dataEnv: "
      ++ show _dataEnv
      ++ ", currentArgOffset: "
      ++ show _currentArgOffset
      ++ ", currentLocalOffset: "
      ++ show _currentLocalOffset
      ++ ", nextTempId: "
      ++ show _nextTempId
      ++ ", staticDataSize: "
      ++ show _staticDataSize
      ++ " }"

globalSymbolTable ::
  SymbolTable
globalSymbolTable =
  SymbolTable
    { _blockEnvs = Map.singleton 0 globalEnv,
      _nextBlockId = 1,
      _dataEnv = globalEnv,
      _currentArgOffset = 16,
      _currentLocalOffset = 0,
      _nextTempId = 0,
      _staticDataSize = 0
    }

insertBlock :: Environment -> SymbolTable -> (BlockId, SymbolTable)
insertBlock env st@SymbolTable {_blockEnvs, _nextBlockId} =
  let newId = _nextBlockId
      newSt =
        st
          { _blockEnvs = Map.insert newId env _blockEnvs,
            _nextBlockId = _nextBlockId + 1
          }
   in (newId, newSt)

lookupBlock :: BlockId -> SymbolTable -> Maybe Environment
lookupBlock blockId SymbolTable {_blockEnvs} = Map.lookup blockId _blockEnvs

currentBlockId :: SymbolTable -> BlockId
currentBlockId symbolTable = (symbolTable ^. nextBlockId) - 1

insertFunToEnv :: BlockId -> FunctionId -> Ty -> SymbolTable -> SymbolTable
insertFunToEnv blockId funId' funTy symbolTable =
  let newSymbol =
        Symbol
          { _symbolId = funId',
            _symbolTy = funTy,
            _symbolStorage = Static,
            _addressTaken = False,
            _stackOffset = Nothing,
            _tempRegister = Nothing,
            _staticOffset = Nothing
          }
   in symbolTable & blockEnvs . ix blockId . envSymbolMap . at funId' ?~ newSymbol

lookupSymbol :: Id -> BlockId -> SymbolTable -> Maybe Symbol
lookupSymbol identifier blockId st =
  st ^? blockEnvs . ix blockId . envSymbolMap . ix identifier
    <|> do
      env <- st ^? blockEnvs . ix blockId
      parentId <- env ^. parentBlockId
      lookupSymbol identifier parentId st

insertVar :: VarDef -> SymbolStorage -> BlockId -> SymbolTable -> SymbolTable
insertVar varDef storage blockId st =
  let newSymbol =
        Symbol
          { _symbolId = _varDefId varDef,
            _symbolTy = _varDefTy varDef,
            _symbolStorage = storage,
            _addressTaken = False,
            _stackOffset = Nothing,
            _tempRegister = Nothing,
            _staticOffset = Nothing
          }
   in st & blockEnvs . ix blockId . envSymbolMap . at (varDef ^. varDefId) ?~ newSymbol

insertArg :: VarDef -> BlockId -> SymbolTable -> SymbolTable
insertArg varDef blockId st@SymbolTable {_blockEnvs} =
  insertVar varDef Argument blockId st {_blockEnvs}

openEnv :: SymbolTable -> (SymbolTable, BlockId)
openEnv st@SymbolTable {_blockEnvs} =
  let newId = _nextBlockId st
      env = emptyEnv newId (currentBlockId st)
      newBlockEnvs = Map.insert newId env _blockEnvs
   in (st {_blockEnvs = newBlockEnvs, _nextBlockId = newId + 1}, newId)

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

getStackOffset :: Id -> BlockId -> SymbolTable -> Maybe Int
getStackOffset identifier blockId st =
  st ^? blockEnvs . ix blockId . envSymbolMap . ix identifier . stackOffset . _Just

getTempRegister :: Id -> BlockId -> SymbolTable -> Maybe Int
getTempRegister identifier blockId st =
  st ^? blockEnvs . ix blockId . envSymbolMap . ix identifier . tempRegister . _Just

getStaticOffset :: Id -> BlockId -> SymbolTable -> Maybe Int
getStaticOffset identifier blockId st =
  st ^? blockEnvs . ix blockId . envSymbolMap . ix identifier . staticOffset . _Just

-- Get all symbols in a block
toList :: BlockId -> SymbolTable -> [Symbol]
toList blockId st = st ^.. blockEnvs . ix blockId . envSymbolMap . traverse

-- Get all data symbols
dataList :: SymbolTable -> [(Id, Symbol)]
dataList st = st ^@.. dataEnv . envSymbolMap . itraversed

setAddressTaken :: Id -> BlockId -> SymbolTable -> SymbolTable
setAddressTaken identifier blockId =
  setSymbolField blockId identifier addressTaken True

updateStackOffset :: Id -> BlockId -> Int -> SymbolTable -> SymbolTable
updateStackOffset identifier blockId offset =
  setSymbolField blockId identifier stackOffset (Just offset)

updateTempRegister :: Id -> BlockId -> Int -> SymbolTable -> SymbolTable
updateTempRegister identifier blockId tempId =
  setSymbolField blockId identifier tempRegister (Just tempId)

-- Stack frame management functions
allocateStackSlot :: Id -> BlockId -> SymbolStorage -> SymbolTable -> SymbolTable
allocateStackSlot identifier blockId storage st =
  case st ^? blockEnvs . ix blockId . envSymbolMap . ix identifier of
    Just symbol ->
      let size = sizeOf (symbol ^. symbolTy)
          (offset, st') = case storage of
            Argument ->
              let newOffset = st ^. currentArgOffset + size
               in (st ^. currentArgOffset, st & currentArgOffset .~ newOffset)
            Auto ->
              let newOffset = st ^. currentLocalOffset - size
               in (newOffset, st & currentLocalOffset .~ newOffset)
            _ -> error $ "Unsupported storage type for stack allocation: " ++ show storage
       in st' & blockEnvs . ix blockId . envSymbolMap . ix identifier . stackOffset ?~ offset
    Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId

allocateTempRegister :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateTempRegister identifier blockId st =
  let tempId = st ^. nextTempId
   in st
        & blockEnvs . ix blockId . envSymbolMap . ix identifier . tempRegister ?~ tempId
        & nextTempId %~ (+ 1)

allocateStaticSlot :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateStaticSlot identifier blockId st =
  case st ^? blockEnvs . ix blockId . envSymbolMap . ix identifier of
    Just symbol ->
      let size = sizeOf (symbol ^. symbolTy)
          offset = st ^. staticDataSize
          updatedSymbol = symbol & staticOffset ?~ offset
       in st
            & blockEnvs . ix blockId . envSymbolMap . ix identifier .~ updatedSymbol
            & dataEnv . envSymbolMap . at identifier ?~ updatedSymbol
            & staticDataSize %~ (+ size)
    Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId

resetFrameAllocation :: SymbolTable -> SymbolTable
resetFrameAllocation st =
  st
    & currentArgOffset .~ 16
    & currentLocalOffset .~ 0

symbolInBlock :: BlockId -> Id -> Traversal' SymbolTable Symbol
symbolInBlock blockId symbolId' = blockEnvs . ix blockId . envSymbolMap . ix symbolId'

-- Modify a symbol if it exists
modifySymbol :: BlockId -> Id -> (Symbol -> Symbol) -> SymbolTable -> SymbolTable
modifySymbol blockId symbolId' f = symbolInBlock blockId symbolId' %~ f

setSymbolField :: BlockId -> Id -> ASetter Symbol Symbol a a -> a -> SymbolTable -> SymbolTable
setSymbolField blockId symbolId' field value =
  symbolInBlock blockId symbolId' . field .~ value
