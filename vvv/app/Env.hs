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
    openEnv,
    SymbolVariant (..),
    insertArg,
  )
where

import Ast (Fun (..), Id, Ty (FunTy), VarDef (..))
import Control.Applicative
-- import Control.Lens
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
  { name :: String, -- Name of the scope (for debugging)
    symbols :: Map.Map Id Symbol -- Symbols in this scope
  }
  deriving (Eq)

instance Show Env where
  show scope =
    "Env { name: "
      ++ scope.name
      ++ ", symbols: "
      ++ show (Map.toList scope.symbols)
      ++ " }"

insert :: Id -> Symbol -> Env -> Env
insert id symbol scope =
  scope {symbols = Map.insert id symbol scope.symbols}

lookup :: Id -> Env -> Maybe Symbol
lookup id scope = Map.lookup id scope.symbols

openEnv :: String -> Env
openEnv name = Env {name, symbols = Map.empty}

-- Function-specific operations
insertFunction :: Fun a b -> Env -> Env
insertFunction Fun {id, args, retty} =
  insert
    id
    Symbol
      { variant = Global,
        ty = FunTy ((\VarDef {ty} -> ty) <$> args) retty
      }

insertVar :: VarDef -> Env -> Env
insertVar VarDef {id, ty} = insert id Symbol {variant = Local, ty}

insertArg :: VarDef -> Env -> Env
insertArg VarDef {id, ty} = insert id Symbol {variant = Argument, ty}
