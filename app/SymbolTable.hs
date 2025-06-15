module SymbolTable
  ( SymbolTable,
    Symbol (..),
    empty,
    insert,
    lookup,
    pushScope,
    popScope,
    lookupFunction,
    insertFunction,
    insertVar,
  )
where

import Ast (Fun (..), Ident, Ty, VarDef (..))
import Data.Map qualified as Map
import Prelude hiding (lookup)

-- Symbol information stored in the table
data Symbol = Variable Ty | Function [Ty] Ty
  deriving (Show, Eq)

-- Symbol table with nested scopes
data SymbolTable a = SymbolTable
  { scopes :: [Map.Map Ident Symbol], -- Stack of scopes (head is current scope)
    functions :: Map.Map Ident (Fun a) -- Global function definitions
  }
  deriving (Show, Eq)

-- Create an empty symbol table
empty :: SymbolTable a
empty = SymbolTable [Map.empty] Map.empty

-- Insert a symbol into the current scope
insert :: Ident -> Symbol -> SymbolTable a -> SymbolTable a
insert name symbol (SymbolTable (currentScope : restScopes) funcs) =
  SymbolTable (Map.insert name symbol currentScope : restScopes) funcs
insert _ _ (SymbolTable [] _) = error "No scope to insert into"

-- Lookup a symbol in all scopes (innermost first)
lookup :: Ident -> SymbolTable a -> Maybe Symbol
lookup name (SymbolTable scopes _) = go scopes
  where
    go [] = Nothing
    go (scope : rest) = case Map.lookup name scope of
      Just symbol -> Just symbol
      Nothing -> go rest

-- Push a new scope onto the stack
pushScope :: SymbolTable a -> SymbolTable a
pushScope (SymbolTable scopes funcs) = SymbolTable (Map.empty : scopes) funcs

-- Pop the current scope from the stack
popScope :: SymbolTable a -> SymbolTable a
popScope (SymbolTable (_ : rest) funcs) = SymbolTable rest funcs
popScope (SymbolTable [] _) = error "Cannot pop from empty scope stack"

-- Function-specific operations
insertFunction :: Fun a -> SymbolTable a -> SymbolTable a
insertFunction fun@(Fun name _ _ _) (SymbolTable scopes funcs) =
  SymbolTable {scopes, functions = Map.insert name fun funcs}

lookupFunction :: Ident -> SymbolTable a -> Maybe (Fun a)
lookupFunction name (SymbolTable _ funcs) = Map.lookup name funcs

-- Helper function to insert a variable from VarDef
insertVar :: VarDef -> SymbolTable a -> SymbolTable a
insertVar (VarDef name ty) = insert name (Variable ty)
