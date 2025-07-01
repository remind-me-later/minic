{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Env
  ( Symbol (..),
    Env (..),
    empty,
    insert,
    lookup,
    insertFunction,
    insertVar,
    pushEnv,
    popEnv,
    peekEnv,
    SymbolAlloc (..),
    insertArg,
    EnvStack (..),
    emptyEnv,
    insertFun,
    emptyEnvStack,
    numSymbolsInEnv,
    toList,
    insertExternFunction,
  )
where

import Ast.Types (ExternFun (..), Fun (..), VarDef (..))
import Control.Applicative (Alternative (empty))
import Data.Map qualified as Map
import TypeSystem (Id, Ty (..))
import Prelude hiding (lookup)

data SymbolAlloc
  = Argument
  | Local
  | Global
  deriving (Eq)

instance Show SymbolAlloc where
  show Argument = "Argument"
  show Local = "Local"
  show Global = "Global"

data Symbol = Symbol {symbolId :: Id, symbolTy :: Ty, symbolAlloc :: SymbolAlloc}
  deriving (Eq)

instance Show Symbol where
  show Symbol {symbolTy, symbolAlloc} =
    "Symbol { ty: " ++ show symbolTy ++ ", alloc: " ++ show symbolAlloc ++ " }"

data Env = Env
  { envName :: String,
    envSymbols :: Map.Map Id Symbol
  }
  deriving (Eq)

instance Show Env where
  show Env {envName, envSymbols} =
    "Env { name: "
      ++ envName
      ++ ", symbols: "
      ++ show (Map.toList envSymbols)
      ++ " }"

newtype EnvStack = EnvStack
  { envStack :: [Env]
  }
  deriving (Eq)

emptyEnv :: String -> Env
emptyEnv name = Env {envName = name, envSymbols = Map.empty}

insert :: Id -> Symbol -> EnvStack -> EnvStack
insert id symbol stack' =
  case stack' of
    EnvStack [] -> error "Cannot insert into an empty environment stack"
    EnvStack (env : rest) ->
      EnvStack {envStack = env {envSymbols = Map.insert id symbol (envSymbols env)} : rest}

lookup :: Id -> EnvStack -> Maybe Symbol
lookup id stack' =
  case stack' of
    EnvStack [] -> Nothing
    EnvStack (env : rest) ->
      case Map.lookup id (envSymbols env) of
        Just symbol -> Just symbol
        Nothing -> lookup id EnvStack {envStack = rest}

pushEnv :: Env -> EnvStack -> EnvStack
pushEnv env EnvStack {envStack} =
  EnvStack {envStack = env : envStack}

popEnv :: EnvStack -> EnvStack
popEnv EnvStack {envStack} =
  case envStack of
    [] -> error "Cannot pop from an empty environment stack"
    _ : rest -> EnvStack {envStack = rest}

peekEnv :: EnvStack -> Env
peekEnv EnvStack {envStack} =
  case envStack of
    [] -> error "Cannot peek into an empty environment stack"
    env : _ -> env

insertFunction :: Fun a b -> EnvStack -> EnvStack
insertFunction Fun {funId, funArgs, funRetTy} =
  insert
    funId
    Symbol
      { symbolId = funId,
        symbolAlloc = Global,
        symbolTy = FunTy {funTyArgs = varDefTy <$> funArgs, funTyRetTy = funRetTy}
      }

insertExternFunction :: ExternFun -> EnvStack -> EnvStack
insertExternFunction ExternFun {externFunId, externFunArgs, externFunRetTy} =
  insert
    externFunId
    Symbol
      { symbolId = externFunId,
        symbolAlloc = Global,
        symbolTy = FunTy {funTyArgs = externFunArgs, funTyRetTy = externFunRetTy}
      }

insertVar :: VarDef -> EnvStack -> EnvStack
insertVar VarDef {varDefId, varDefTy} =
  insert
    varDefId
    Symbol
      { symbolId = varDefId,
        symbolAlloc = Local,
        symbolTy = varDefTy
      }

insertArg :: VarDef -> EnvStack -> EnvStack
insertArg VarDef {varDefId, varDefTy} =
  insert
    varDefId
    Symbol
      { symbolId = varDefId,
        symbolAlloc = Argument,
        symbolTy = varDefTy
      }

insertFun :: Fun a b -> EnvStack -> EnvStack
insertFun Fun {funId, funArgs, funRetTy} =
  insert
    funId
    Symbol
      { symbolId = funId,
        symbolAlloc = Global,
        symbolTy = FunTy {funTyArgs = varDefTy <$> funArgs, funTyRetTy = funRetTy}
      }

emptyEnvStack :: String -> EnvStack
emptyEnvStack name = EnvStack {envStack = [emptyEnv name]}

numSymbolsInEnv :: Env -> Int
numSymbolsInEnv e = Map.size (envSymbols e)

toList :: Env -> [Symbol]
toList e = Map.elems (envSymbols e)