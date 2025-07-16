{-# LANGUAGE LambdaCase #-}

module SymbolTable
  ( BlockId,
    FunctionId,
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
    allocateTempRegister,
    getStackOffset,
    getTempRegister,
    getStaticOffset,
    allocateStaticSlot,
    resetFrameAllocation,
    dataList,
    insertExternFunToEnv,
    allocateAutoVarStackSlot,
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
        FunSymbol
          { _funSymbolId = funId',
            _funSymbolTy = funTy,
            _funSymbolStorage = FunNormal
          }
   in symbolTable & blockEnvs . ix parentBlockId' . envSymbolMap . at funId' ?~ newSymbol

insertExternFunToEnv :: BlockId -> FunctionId -> Ty -> SymbolTable -> SymbolTable
insertExternFunToEnv parentBlockId' funId' funTy symbolTable =
  let newSymbol =
        FunSymbol
          { _funSymbolId = funId',
            _funSymbolTy = funTy,
            _funSymbolStorage = FunExtern
          }
   in symbolTable & blockEnvs . ix parentBlockId' . envSymbolMap . at funId' ?~ newSymbol

lookupSymbol :: Id -> BlockId -> SymbolTable -> Maybe Symbol
lookupSymbol identifier blockId' st =
  st ^? blockEnvs . ix blockId' . envSymbolMap . ix identifier
    <|> do
      env <- st ^? blockEnvs . ix blockId'
      parentId <- env ^. parentBlockId
      lookupSymbol identifier parentId st

insertVar :: VarDef -> BlockId -> SymbolTable -> SymbolTable
insertVar varDef blockId' st =
  let newSymbol =
        VarSymbol
          { _varSymbolId = varDef ^. varDefId,
            _varSymbolTy = varDef ^. varDefTy,
            _varSymbolStorage = Nothing,
            _varAddressTaken = False
          }
   in st & blockEnvs . ix blockId' . envSymbolMap . at (varDef ^. varDefId) ?~ newSymbol

insertArg :: VarDef -> BlockId -> SymbolTable -> SymbolTable
insertArg varDef blockId' st@SymbolTable {_blockEnvs} =
  let newSymbol =
        ArgSymbol
          { _argSymbolId = varDef ^. varDefId,
            _argSymbolTy = varDef ^. varDefTy,
            _argSymbolStorage = ArgNormal (st ^. currentArgOffset)
          }
      newSt =
        st
          & blockEnvs . ix blockId' . envSymbolMap . at (varDef ^. varDefId) ?~ newSymbol
          & currentArgOffset %~ (+ sizeOf (varDef ^. varDefTy))
   in newSt

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
getStackOffset identifier blockId' st = do
  let symb = st ^? symbolInBlock blockId' identifier
  case symb of
    Just symb' -> case symb' of
      VarSymbol {_varSymbolStorage} ->
        _varSymbolStorage >>= \case
          (VarAutoStack offset) -> Just offset
          _ -> Nothing
      ArgSymbol {_argSymbolStorage} -> Just $ argSymbolStorageOffset _argSymbolStorage
      FunSymbol {} -> error $ "Cannot get stack offset for function symbol: " ++ show identifier
    Nothing -> do
      env <- st ^? blockEnvs . ix blockId'
      case env ^. parentBlockId of
        Just parentId -> getStackOffset identifier parentId st
        Nothing -> Nothing

getTempRegister :: Id -> BlockId -> SymbolTable -> Maybe Int
getTempRegister identifier blockId' st = do
  let symb = st ^? symbolInBlock blockId' identifier

  case symb of
    Just symb' ->
      case symb' of
        VarSymbol {_varSymbolStorage} ->
          _varSymbolStorage >>= \case
            (VarAutoTemp tempReg) -> Just tempReg
            _ -> Nothing
        ArgSymbol {_argSymbolStorage} -> Nothing -- Arguments do not have temp registers
        FunSymbol {} -> error $ "Cannot get temp register for function symbol: " ++ show identifier
    Nothing -> do
      env <- st ^? blockEnvs . ix blockId'
      case env ^. parentBlockId of
        Just parentId -> getTempRegister identifier parentId st
        Nothing -> Nothing

getStaticOffset :: Id -> BlockId -> SymbolTable -> Maybe Int
getStaticOffset identifier blockId' st = do
  let symb = st ^? dataEnv . envSymbolMap . ix identifier
  case symb of
    Just symb' -> case symb' of
      VarSymbol {_varSymbolStorage} ->
        _varSymbolStorage >>= \case
          (VarStatic offset) -> Just offset
          _ -> Nothing
      ArgSymbol {_argSymbolStorage} -> Nothing -- Arguments do not have static offsets
      FunSymbol {} -> error $ "Cannot get static offset for function symbol: " ++ show identifier
    Nothing -> do
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
setAddressTaken identifier blockId' symbolTable' =
  case lookupSymbol identifier blockId' symbolTable' of
    Just symbol ->
      case symbol of
        VarSymbol {_varAddressTaken} ->
          symbolTable'
            & blockEnvs . ix blockId' . envSymbolMap . ix identifier . varAddressTaken .~ True
        ArgSymbol {} -> error $ "Cannot set address taken for argument symbol: " ++ identifier
        FunSymbol {} -> error $ "Cannot set address taken for function symbol: " ++ identifier
    Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId'

-- Stack frame management functions
allocateAutoVarStackSlot :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateAutoVarStackSlot identifier blockId' st =
  case st ^? blockEnvs . ix blockId' . envSymbolMap . ix identifier of
    Just symbol ->
      case symbol of
        s@VarSymbol {_varSymbolStorage} ->
          case _varSymbolStorage of
            Nothing ->
              let size = sizeOf (_varSymbolTy s)
                  newOffset = st ^. currentLocalOffset - size
                  updatedSymbol = symbol & varSymbolStorage ?~ VarAutoStack newOffset
               in st
                    & blockEnvs . ix blockId' . envSymbolMap . ix identifier .~ updatedSymbol
                    & currentLocalOffset .~ newOffset
            Just (VarAutoStack _) ->
              error $ "Symbol " ++ identifier ++ " already has an auto stack slot allocated."
            Just (VarStatic _) ->
              error $ "Cannot allocate auto stack slot for static symbol: " ++ identifier
            Just (VarAutoTemp _) ->
              error $ "Cannot allocate auto stack slot for temp symbol: " ++ identifier
        ArgSymbol {_argSymbolStorage} -> error $ "Cannot allocate auto stack slot for argument symbol: " ++ show identifier
        FunSymbol {} -> error $ "Cannot allocate auto stack slot for function symbol: " ++ show identifier
    Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId'

-- allocateFunArgStackSlot :: Id -> BlockId -> SymbolTable -> SymbolTable
-- allocateFunArgStackSlot identifier blockId' st =
--   case st ^? blockEnvs . ix blockId' . envSymbolMap . ix identifier of
--     Just symbol ->
--       let size = sizeOf (symbol ^. symbolTy)
--           newOffset = st ^. currentArgOffset + size
--        in st
--             & blockEnvs . ix blockId' . envSymbolMap . ix identifier . stackOffset ?~ st ^. currentArgOffset
--             & currentArgOffset .~ newOffset
--     Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId'

allocateTempRegister :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateTempRegister identifier blockId' st =
  case st ^? blockEnvs . ix blockId' . envSymbolMap . ix identifier of
    Just symbol ->
      case symbol of
        VarSymbol {_varSymbolStorage} ->
          case _varSymbolStorage of
            Nothing ->
              let tempId = st ^. nextTempId
                  updatedSymbol = symbol & varSymbolStorage ?~ VarAutoTemp tempId
               in st
                    & blockEnvs . ix blockId' . envSymbolMap . ix identifier .~ updatedSymbol
                    & nextTempId %~ (+ 1)
            Just (VarAutoStack _) ->
              error $ "Cannot allocate temp register for auto stack symbol: " ++ identifier
            Just (VarStatic _) ->
              error $ "Cannot allocate temp register for static symbol: " ++ identifier
            Just (VarAutoTemp _) ->
              error $ "Symbol " ++ identifier ++ " already has a temp register allocated."
        ArgSymbol {_argSymbolStorage} -> error $ "Cannot allocate temp register for argument symbol: " ++ show identifier
        FunSymbol {} -> error $ "Cannot allocate temp register for function symbol: " ++ show identifier
    Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId'

allocateStaticSlot :: Id -> BlockId -> SymbolTable -> SymbolTable
allocateStaticSlot identifier blockId' st =
  case st ^? blockEnvs . ix blockId' . envSymbolMap . ix identifier of
    Just symbol ->
      case symbol of
        VarSymbol {_varSymbolStorage} ->
          case _varSymbolStorage of
            Nothing ->
              let size = sizeOf (_varSymbolTy symbol)
                  offset = st ^. staticDataSize
                  updatedSymbol = symbol & varSymbolStorage ?~ VarStatic offset
               in st
                    & blockEnvs . ix blockId' . envSymbolMap . ix identifier .~ updatedSymbol
                    & dataEnv . envSymbolMap . at identifier ?~ updatedSymbol
                    & staticDataSize %~ (+ size)
            Just (VarStatic offset) ->
              error $ "Symbol " ++ identifier ++ " already has a static slot at offset " ++ show offset
            Just (VarAutoStack _) ->
              error $ "Cannot allocate static slot for auto stack symbol: " ++ identifier
            Just (VarAutoTemp _) ->
              error $ "Cannot allocate static slot for auto temp symbol: " ++ identifier
        ArgSymbol {_argSymbolStorage} -> error $ "Cannot allocate static slot for argument symbol: " ++ show identifier
        FunSymbol {} -> error $ "Cannot allocate static slot for function symbol: " ++ show identifier
    Nothing -> error $ "Symbol " ++ identifier ++ " not found in block " ++ show blockId'

resetFrameAllocation :: SymbolTable -> SymbolTable
resetFrameAllocation st =
  st
    & currentArgOffset .~ 16
    & currentLocalOffset .~ 0

symbolInBlock :: BlockId -> Id -> Traversal' SymbolTable Symbol
symbolInBlock blockId' symbolId' = blockEnvs . ix blockId' . envSymbolMap . ix symbolId'
