{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Scope
  ( Symbol (..),
    Scope (..),
    empty,
    insert,
    lookup,
    insertFunction,
    insertVar,
    openScope,
    SymbolVariant (..),
    insertArg,
  )
where

import Ast (Fun (..), Id, Ty (FunTy), VarDef (..))
import Control.Applicative
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

data Scope = Scope
  { name :: String, -- Name of the scope (for debugging)
    symbols :: Map.Map Id Symbol, -- Symbols in this scope
    parent :: Maybe Scope -- Optional parent scope for nested scopes
  }
  deriving (Eq)

instance Show Scope where
  show scope =
    "Scope { name: "
      ++ scope.name
      ++ ", symbols: "
      ++ show (Map.toList scope.symbols)
      ++ ", parent: "
      ++ show (fmap (\p -> p.name) scope.parent)
      ++ " }"

insert :: Id -> Symbol -> Scope -> Scope
insert id symbol scope =
  scope {symbols = Map.insert id symbol scope.symbols}

lookup :: Id -> Scope -> Maybe Symbol
lookup id scope =
  Map.lookup id scope.symbols <|> case scope.parent of
    Just parentScope -> lookup id parentScope -- Search in parent scope if exists
    Nothing -> Nothing -- No parent scope, return Nothing

openScope :: String -> Scope -> Scope
openScope name scope = Scope {name, symbols = Map.empty, parent = Just scope}

-- Function-specific operations
insertFunction :: Fun a b -> Scope -> Scope
insertFunction Fun {id, args, retty} =
  insert
    id
    Symbol
      { variant = Global,
        ty = FunTy ((\VarDef {ty} -> ty) <$> args) retty
      }

insertVar :: VarDef -> Scope -> Scope
insertVar VarDef {id, ty} = insert id Symbol {variant = Local, ty}

insertArg :: VarDef -> Scope -> Scope
insertArg VarDef {id, ty} = insert id Symbol {variant = Argument, ty}
