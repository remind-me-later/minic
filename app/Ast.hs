module Ast
  ( Expr (..),
    Stmt (..),
    Block (..),
    Fun (..),
    Program (..),
    Operator (..),
    Ty (..),
    Ident,
    VarDef (..),
  )
where

type Ident = String

data Operator
  = Add
  | Subtract
  | Multiply
  | Assign
  | Equal
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | NotEqual
  | And
  | Or
  | Not
  | Xor
  | Modulo
  deriving (Show, Eq)

data Expr
  = BinExpr Expr Operator Expr
  | NumberExpr Int
  | IdentifierExpr Ident
  | Call
      { funCallName :: Ident,
        funCallArgs :: [Expr]
      }
  deriving (Show, Eq)

data Ty
  = I32
  | Bool
  | Void
  deriving (Show, Eq)

data VarDef = VarDef Ident Ty deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | LetStmt VarDef Expr
  | AssignStmt Ident Expr
  | ReturnStmt Expr
  | IfStmt {ifCond :: Expr, ifBody :: Block, elseBody :: Maybe Block}
  | WhileStmt {whileCond :: Expr, whileBody :: Block}
  deriving (Show, Eq)

newtype Block = Block [Stmt] deriving (Show, Eq)

data Fun = Fun {funName :: Ident, funArgs :: [VarDef], funRetTy :: Ty, funBody :: Block} deriving (Show, Eq)

newtype Program = Program [Fun] deriving (Show, Eq)