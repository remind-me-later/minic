module SymbolTable.Types where

import Data.Map qualified as Map
import TypeSystem (Id, Ty)

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

instance Show Symbol where
  show
    Symbol
      { _symbolTy,
        _symbolStorage,
        _symbolId,
        _addressTaken,
        _stackOffset,
        _tempRegister,
        _staticOffset
      } =
      "Symbol { id: "
        ++ show _symbolId
        ++ ", type: "
        ++ show _symbolTy
        ++ ", storage: "
        ++ show _symbolStorage
        ++ ", addressTaken: "
        ++ show _addressTaken
        ++ ", stackOffset: "
        ++ show _stackOffset
        ++ ", tempRegister: "
        ++ show _tempRegister
        ++ ", staticOffset: "
        ++ show _staticOffset
        ++ " }"

data Environment = Env
  { _envBlockId :: BlockId,
    _envSymbolMap :: Map.Map Id Symbol,
    _parentBlockId :: Maybe BlockId
  }
  deriving (Eq)

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
    _dataEnv :: Environment,
    _currentArgOffset :: Int,
    _currentLocalOffset :: Int,
    _nextTempId :: Int,
    _staticDataSize :: Int
  }
  deriving (Eq)

instance Show SymbolTable where
  show SymbolTable {_blockEnvs, _dataEnv, _currentArgOffset, _currentLocalOffset, _nextTempId, _staticDataSize} =
    "block environments:\n"
      ++ unlineBlockEnvs _blockEnvs
      ++ "\n"
      ++ "data environment: "
      ++ show _dataEnv
      ++ "\n"
      ++ "current argument offset: "
      ++ show _currentArgOffset
      ++ "\n"
      ++ "current local offset: "
      ++ show _currentLocalOffset
      ++ "\n"
      ++ "next temporary ID: "
      ++ show _nextTempId
      ++ "\n"
      ++ "static data size: "
      ++ show _staticDataSize
      ++ "\n"
    where
      unlineBlockEnvs =
        Map.foldrWithKey
          (\k v acc -> acc ++ "Block " ++ show k ++ ": " ++ show v ++ "\n")
          ""

globalSymbolTable ::
  SymbolTable
globalSymbolTable =
  SymbolTable
    { _blockEnvs = Map.singleton 0 globalEnv,
      _dataEnv = globalEnv,
      _currentArgOffset = 16,
      _currentLocalOffset = 0,
      _nextTempId = 0,
      _staticDataSize = 0
    }