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

import Ast.Lenses
import Ast.Types (VarDef (..))
import Control.Applicative ((<|>))
import Control.Lens
import Data.Map qualified as Map
import SymbolTable.Lenses
import SymbolTable.Types
import TypeSystem (Id, Ty (..), sizeOf)

insertBlock :: BlockId -> Environment -> SymbolTable -> SymbolTable
insertBlock blockId' env st@SymbolTable {_blockEnvs} =
  let newSt =
        st
          { _blockEnvs = Map.insert blockId' env _blockEnvs
          }
   in newSt

lookupBlock :: BlockId -> SymbolTable -> Maybe Environment
lookupBlock blockId' SymbolTable {_blockEnvs} = Map.lookup blockId' _blockEnvs

insertFunToEnv :: BlockId -> FunctionId -> Ty -> SymbolTable -> SymbolTable
insertFunToEnv parentBlockId' funId' funTy symbolTable =
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
   in symbolTable & blockEnvs . ix parentBlockId' . envSymbolMap . at funId' ?~ newSymbol

lookupSymbol :: Id -> BlockId -> SymbolTable -> Maybe Symbol
lookupSymbol identifier blockId' st =
  st ^? blockEnvs . ix blockId' . envSymbolMap . ix identifier
    <|> do
      env <- st ^? blockEnvs . ix blockId'
      parentId <- env ^. parentBlockId
      lookupSymbol identifier parentId st

insertVar :: VarDef -> SymbolStorage -> BlockId -> SymbolTable -> SymbolTable
insertVar varDef storage blockId' st =
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
   in st & blockEnvs . ix blockId' . envSymbolMap . at (varDef ^. varDefId) ?~ newSymbol

insertArg :: VarDef -> BlockId -> SymbolTable -> SymbolTable
insertArg varDef blockId' st@SymbolTable {_blockEnvs} =
  insertVar varDef Argument blockId' st {_blockEnvs}

openEnv :: BlockId -> BlockId -> SymbolTable -> SymbolTable
openEnv blockId' parentBlockId' st@SymbolTable {_blockEnvs} =
  let env = emptyEnv blockId' parentBlockId'
      newBlockEnvs =
        if Map.member blockId' _blockEnvs
          then _blockEnvs
          else Map.insert blockId' env _blockEnvs
   in st {_blockEnvs = newBlockEnvs}

prevEnv :: BlockId -> SymbolTable -> BlockId
prevEnv blockId' SymbolTable {_blockEnvs} =
  case Map.lookup blockId' _blockEnvs of
    Just env -> case _parentBlockId env of
      Just parentId -> parentId
      Nothing -> error $ "No parent block for block ID " ++ show blockId'
    Nothing -> error $ "Block with ID " ++ show blockId' ++ " not found in symbol table."

peekEnv :: BlockId -> SymbolTable -> Environment
peekEnv blockId' SymbolTable {_blockEnvs} =
  case Map.lookup blockId' _blockEnvs of
    Just env -> env
    Nothing -> error $ "Block with ID " ++ show blockId' ++ " not found in symbol table."

getStackOffset :: Id -> BlockId -> SymbolTable -> Maybe Int
getStackOffset identifier blockId' st =
  st ^? symbolInBlock blockId' identifier . stackOffset . _Just
    <|> do
      env <- st ^? blockEnvs . ix blockId'
      case env ^. parentBlockId of
        Just parentId -> getStackOffset identifier parentId st
        Nothing -> Nothing

getTempRegister :: Id -> BlockId -> SymbolTable -> Maybe Int
getTempRegister identifier blockId' st =
  st ^? symbolInBlock blockId' identifier . tempRegister . _Just
    <|> do
      env <- st ^? blockEnvs . ix blockId'
      case env ^. parentBlockId of
        Just parentId -> getTempRegister identifier parentId st
        Nothing -> Nothing

getStaticOffset :: Id -> BlockId -> SymbolTable -> Maybe Int
getStaticOffset identifier blockId' st =
  st ^? dataEnv . envSymbolMap . ix identifier . staticOffset . _Just
    <|> do
      env <- st ^? blockEnvs . ix blockId'
      case env ^. parentBlockId of
        Just parentId -> getStaticOffset identifier parentId st
        Nothing -> Nothing

-- Get all symbols in a block
toList :: BlockId -> SymbolTable -> [Symbol]
toList blockId' st = st ^.. blockEnvs . ix blockId' . envSymbolMap . traverse

-- Get all data symbols
dataList :: SymbolTable -> [(Id, Symbol)]
dataList st = st ^@.. dataEnv . envSymbolMap . itraversed

setAddressTaken :: Id -> BlockId -> SymbolTable -> SymbolTable
setAddressTaken identifier blockId' =
  setSymbolField blockId' identifier addressTaken True

-- Stack frame management functions
allocateStackSlot :: Id -> BlockId -> SymbolStorage -> SymbolTable -> SymbolTable
allocateStackSlot identifier blockId' storage st =
  case st ^? blockEnvs . ix blockId' . envSymbolMap . ix identifier of
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
       in st' & blockEnvs . ix blockId' . envSymbolMap . ix identifier . stackOffset ?~ offset
    Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId'

allocateTempRegister :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateTempRegister identifier blockId' st =
  let tempId = st ^. nextTempId
   in st
        & blockEnvs . ix blockId' . envSymbolMap . ix identifier . tempRegister ?~ tempId
        & nextTempId %~ (+ 1)

allocateStaticSlot :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateStaticSlot identifier blockId' st =
  case st ^? blockEnvs . ix blockId' . envSymbolMap . ix identifier of
    Just symbol ->
      let size = sizeOf (symbol ^. symbolTy)
          offset = st ^. staticDataSize
          updatedSymbol = symbol & staticOffset ?~ offset
       in st
            & blockEnvs . ix blockId' . envSymbolMap . ix identifier .~ updatedSymbol
            & dataEnv . envSymbolMap . at identifier ?~ updatedSymbol
            & staticDataSize %~ (+ size)
    Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId'

resetFrameAllocation :: SymbolTable -> SymbolTable
resetFrameAllocation st =
  st
    & currentArgOffset .~ 16
    & currentLocalOffset .~ 0

symbolInBlock :: BlockId -> Id -> Traversal' SymbolTable Symbol
symbolInBlock blockId' symbolId' = blockEnvs . ix blockId' . envSymbolMap . ix symbolId'

setSymbolField :: BlockId -> Id -> ASetter Symbol Symbol a a -> a -> SymbolTable -> SymbolTable
setSymbolField blockId' symbolId' field value =
  symbolInBlock blockId' symbolId' . field .~ value
