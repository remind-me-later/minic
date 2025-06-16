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
  { scopeName :: String, -- Name of the scope (for debugging)
    symbols :: Map.Map Ident Symbol, -- Symbols in this scope
    parent :: Maybe Scope -- Optional parent scope for nested scopes
  }
  deriving (Eq)

instance Show Scope where
  show scope = "Scope: " ++ scopeName scope ++ ", Symbols: " ++ show (Map.keys (symbols scope))

newGlobalScope :: Scope
newGlobalScope = Scope {scopeName = "global", symbols = Map.empty, parent = Nothing}

insert :: Ident -> Symbol -> Scope -> Scope
insert name symbol scope =
  scope {symbols = Map.insert name symbol (symbols scope)}

lookup :: Ident -> Scope -> Maybe Symbol
lookup name scope =
  Map.lookup name (symbols scope) <|> case parent scope of
    Just parentScope -> lookup name parentScope -- Search in parent scope if exists
    Nothing -> Nothing -- No parent scope, return Nothing

openScope :: String -> Scope -> Scope
openScope scopeName scope = Scope {scopeName, symbols = Map.empty, parent = Just scope}

-- Function-specific operations
insertFunction :: Fun a -> Scope -> Scope
insertFunction (Fun _ name args retTy _) = insert name Symbol {ty = FunTy (map (\(VarDef _ t) -> t) args) retTy}

-- Helper function to insert a variable from VarDef
insertVar :: VarDef -> Scope -> Scope
insertVar (VarDef name ty) = insert name Symbol {ty}
