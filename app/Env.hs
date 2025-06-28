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

data Symbol = Symbol {id :: Id, ty :: Ty, alloc :: SymbolAlloc}
  deriving (Eq)

instance Show Symbol where
  show Symbol {ty, alloc} =
    "Symbol { ty: " ++ show ty ++ ", alloc: " ++ show alloc ++ " }"

data Env = Env
  { name :: String,
    symbols :: Map.Map Id Symbol
  }
  deriving (Eq)

instance Show Env where
  show env =
    "Env { name: "
      ++ env.name
      ++ ", symbols: "
      ++ show (Map.toList env.symbols)
      ++ " }"

newtype EnvStack = EnvStack
  { stack :: [Env]
  }
  deriving (Eq)

emptyEnv :: String -> Env
emptyEnv name = Env {name, symbols = Map.empty}

insert :: Id -> Symbol -> EnvStack -> EnvStack
insert id symbol stack' =
  case stack' of
    EnvStack [] -> error "Cannot insert into an empty environment stack"
    EnvStack (env : rest) ->
      EnvStack {stack = env {symbols = Map.insert id symbol env.symbols} : rest}

lookup :: Id -> EnvStack -> Maybe Symbol
lookup id stack' =
  case stack' of
    EnvStack [] -> Nothing
    EnvStack (env : rest) ->
      case Map.lookup id env.symbols of
        Just symbol -> Just symbol
        Nothing -> lookup id (EnvStack {stack = rest})

pushEnv :: Env -> EnvStack -> EnvStack
pushEnv env es = EnvStack {stack = env : es.stack}

popEnv :: EnvStack -> EnvStack
popEnv es =
  case es.stack of
    [] -> error "Cannot pop from an empty environment stack"
    _ : rest -> EnvStack {stack = rest}

peekEnv :: EnvStack -> Env
peekEnv es =
  case es.stack of
    [] -> error "Cannot peek into an empty environment stack"
    env : _ -> env

insertFunction :: Fun a b -> EnvStack -> EnvStack
insertFunction f =
  insert
    f.id
    Symbol
      { id = f.id,
        alloc = Global,
        ty = FunTy {args = (.ty) <$> f.args, retty = f.retty}
      }

insertExternFunction :: ExternFun -> EnvStack -> EnvStack
insertExternFunction f =
  insert
    f.id
    Symbol
      { id = f.id,
        alloc = Global,
        ty = FunTy {args = f.args, retty = f.retty}
      }

insertVar :: VarDef -> EnvStack -> EnvStack
insertVar v = insert v.id Symbol {id = v.id, alloc = Local, ty = v.ty}

insertArg :: VarDef -> EnvStack -> EnvStack
insertArg v = insert v.id Symbol {id = v.id, alloc = Argument, ty = v.ty}

insertFun :: Fun a b -> EnvStack -> EnvStack
insertFun f =
  insert
    f.id
    Symbol
      { id = f.id,
        alloc = Global,
        ty = FunTy {args = (.ty) <$> f.args, retty = f.retty}
      }

emptyEnvStack :: String -> EnvStack
emptyEnvStack name = EnvStack {stack = [emptyEnv name]}

numSymbolsInEnv :: Env -> Int
numSymbolsInEnv e = Map.size e.symbols

toList :: Env -> [Symbol]
toList e = Map.elems e.symbols