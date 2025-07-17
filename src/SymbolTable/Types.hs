module SymbolTable.Types where

import Data.Map qualified as Map
import TypeSystem (Id, Ty)

type BlockId = Int

type FunctionId = String

data VarSymbolStorage
  = VarAutoStack
      { varSymbolStorageStackOffset :: Int
      }
  | VarAutoTemp
      { varSymbolStorageTempRegister :: Int
      }
  | VarStatic
      { varSymbolStorageStaticOffset :: Int
      }
  deriving (Eq)

instance Show VarSymbolStorage where
  show (VarAutoStack {varSymbolStorageStackOffset}) =
    "VarAutoStack { stackOffset: " ++ show varSymbolStorageStackOffset ++ " }"
  show (VarAutoTemp {varSymbolStorageTempRegister}) =
    "VarAutoTemp { tempRegister: " ++ show varSymbolStorageTempRegister ++ " }"
  show (VarStatic {varSymbolStorageStaticOffset}) =
    "VarStatic { staticOffset: " ++ show varSymbolStorageStaticOffset ++ " }"

data FunSymbolStorage
  = FunNormal -- TODO: Are functions static? Whatever, won't matter until we want to take function addresses
  | FunExtern
  deriving (Eq)

instance Show FunSymbolStorage where
  show FunNormal = "FunNormal"
  show FunExtern = "FunExtern"

newtype ArgSymbolStorage
  = ArgNormal
  { argSymbolStorageOffset :: Int -- right now arguments are always on the stack
  }
  deriving (Eq)

instance Show ArgSymbolStorage where
  show (ArgNormal offset) = "ArgNormal { offset: " ++ show offset ++ " }"

data Symbol
  = VarSymbol
      { _varSymbolId :: Id,
        _varSymbolTy :: Ty,
        _varSymbolStorage :: VarSymbolStorage,
        _varAddressTaken :: Bool
      }
  | ArgSymbol
      { _argSymbolId :: Id,
        _argSymbolTy :: Ty,
        _argSymbolStorage :: ArgSymbolStorage
      }
  | FunSymbol
      { _funSymbolId :: FunctionId,
        _funSymbolTy :: Ty,
        _funSymbolStorage :: FunSymbolStorage
      }
  deriving (Eq)

instance Show Symbol where
  show
    ( VarSymbol
        { _varSymbolId,
          _varSymbolTy,
          _varSymbolStorage
        }
      ) =
      "VarSymbol { id: "
        ++ show _varSymbolId
        ++ ", type: "
        ++ show _varSymbolTy
        ++ ", storage: "
        ++ show _varSymbolStorage
        ++ " }"
  show
    ( ArgSymbol
        { _argSymbolId,
          _argSymbolTy,
          _argSymbolStorage
        }
      ) =
      "ArgSymbol { id: "
        ++ show _argSymbolId
        ++ ", type: "
        ++ show _argSymbolTy
        ++ ", storage: "
        ++ show _argSymbolStorage
        ++ " }"
  show
    ( FunSymbol
        { _funSymbolId,
          _funSymbolTy,
          _funSymbolStorage
        }
      ) =
      "FunSymbol { id: "
        ++ show _funSymbolId
        ++ ", type: "
        ++ show _funSymbolTy
        ++ ", storage: "
        ++ show _funSymbolStorage
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