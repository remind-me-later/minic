{-# LANGUAGE TemplateHaskell #-}

module Ast.Lenses where

import Ast.Types
import Control.Lens

-- Generate lenses for all record fields
makeLenses ''Exp
makeLenses ''VarDef
makeLenses ''Block
makeLenses ''Fun
makeLenses ''ExternFun
makeLenses ''Program

-- Manual lenses for ExpInner since it's a sum type
-- You can create prisms for pattern matching
makePrisms ''ExpInner

-- Helper traversals for common operations
-- Traverse all expressions in a statement
stmtExps :: Traversal' (Stmt ea ba) (Exp ea)
stmtExps f stmt = case stmt of
  ExpStmt exp' -> ExpStmt <$> f exp'
  LetStmt vd exp' storage -> LetStmt vd <$> f exp' <*> pure storage
  LetArrStmt vd size exps storage -> LetArrStmt vd size <$> traverse f exps <*> pure storage
  AssignStmt id' exp' -> AssignStmt id' <$> f exp'
  AssignArrStmt id' idx exp' -> AssignArrStmt id' <$> f idx <*> f exp'
  ReturnStmt (Just exp') -> ReturnStmt . Just <$> f exp'
  ReturnStmt Nothing -> pure (ReturnStmt Nothing)
  IfStmt cond body elseBody -> IfStmt <$> f cond <*> pure body <*> pure elseBody
  WhileStmt cond body -> WhileStmt <$> f cond <*> pure body
  ForStmt init' cond update body -> (ForStmt init' <$> f cond) <*> pure update <*> pure body

-- Traverse all expressions in a block
blockExps :: Traversal' (Block ea ba) (Exp ea)
blockExps = blockStmts . traverse . stmtExps

-- Traverse all expressions in a function
funExps :: Traversal' (Fun ea ba) (Exp ea)
funExps = funBody . blockExps