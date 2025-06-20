{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    SymbolVariant (..),
    insertArg,
    EnvStack (..),
    emptyEnv,
    insertFun,
    emptyEnvStack,
  )
where

import Ast.Types (Fun (..), Id, Ty (..), VarDef (..))
import Control.Applicative (Alternative (empty))
import Data.Map qualified as Map
import Prelude hiding (lookup)

data SymbolVariant = Argument | Local | Global
  deriving (Eq)

instance Show SymbolVariant where
  show Argument = "Argument"
  show Local = "Local"
  show Global = "Global"

data Symbol = Symbol {ty :: Ty, variant :: SymbolVariant}
  deriving (Eq)

instance Show Symbol where
  show Symbol {ty, variant} =
    "Symbol { ty: " ++ show ty ++ ", variant: " ++ show variant ++ " }"

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
emptyEnv name = Env {name = name, symbols = Map.empty}

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
pushEnv env EnvStack {stack} =
  EnvStack {stack = env : stack}

popEnv :: EnvStack -> EnvStack
popEnv EnvStack {stack} =
  case stack of
    [] -> error "Cannot pop from an empty environment stack"
    _ : rest -> EnvStack {stack = rest}

peekEnv :: EnvStack -> Env
peekEnv EnvStack {stack} =
  case stack of
    [] -> error "Cannot peek into an empty environment stack"
    env : _ -> env

insertFunction :: Fun a b -> EnvStack -> EnvStack
insertFunction Fun {id, args, retty} =
  insert
    id
    Symbol
      { variant = Global,
        ty = FunTy ((\VarDef {ty} -> ty) <$> args) retty
      }

insertVar :: VarDef -> EnvStack -> EnvStack
insertVar VarDef {id, ty} = insert id Symbol {variant = Local, ty}

insertArg :: VarDef -> EnvStack -> EnvStack
insertArg VarDef {id, ty} = insert id Symbol {variant = Argument, ty}

insertFun :: Fun a b -> EnvStack -> EnvStack
insertFun Fun {id, args, retty} =
  insert
    id
    Symbol
      { variant = Global,
        ty = FunTy {args = (\VarDef {ty} -> ty) <$> args, retty}
      }

emptyEnvStack :: String -> EnvStack
emptyEnvStack name = EnvStack {stack = [emptyEnv name]}