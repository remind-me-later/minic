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
    newGlobalScope,
    openScope,
  )
where

import Ast (Fun (..), Ident, Ty (FunTy), VarDef (..))
import Control.Applicative
import Data.Map qualified as Map
import Prelude hiding (lookup)

-- Symbol information stored in the table
data Symbol = Symbol {ty :: Ty}
  deriving (Show, Eq)

data Scope = Scope
  { name :: String, -- Name of the scope (for debugging)
    symbols :: Map.Map Ident Symbol, -- Symbols in this scope
    parent :: Maybe Scope -- Optional parent scope for nested scopes
  }
  deriving (Eq)

instance Show Scope where
  show scope = "Scope: " ++ scope.name ++ ", Symbols: " ++ show (Map.keys scope.symbols)

newGlobalScope :: Scope
newGlobalScope = Scope {name = "global", symbols = Map.empty, parent = Nothing}

insert :: Ident -> Symbol -> Scope -> Scope
insert id symbol scope =
  scope {symbols = Map.insert id symbol scope.symbols}

lookup :: Ident -> Scope -> Maybe Symbol
lookup id scope =
  Map.lookup id scope.symbols <|> case scope.parent of
    Just parentScope -> lookup id parentScope -- Search in parent scope if exists
    Nothing -> Nothing -- No parent scope, return Nothing

openScope :: String -> Scope -> Scope
openScope name scope = Scope {name, symbols = Map.empty, parent = Just scope}

-- Function-specific operations
insertFunction :: Fun a b -> Scope -> Scope
insertFunction Fun {id, args, retty} = insert id Symbol {ty = FunTy ((\VarDef {ty} -> ty) <$> args) retty}

-- Helper function to insert a variable from VarDef
insertVar :: VarDef -> Scope -> Scope
insertVar VarDef {id, ty} = insert id Symbol {ty}
