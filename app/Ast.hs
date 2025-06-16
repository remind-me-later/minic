{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
    RawExpr,
    RawStmt,
    RawBlock,
    RawFun,
    RawProgram,
    exprAnnot,
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

data Expr ea
  = BinExpr
      { annot :: ea,
        left :: Expr ea,
        op :: Operator,
        right :: Expr ea
      }
  | NumberExpr
      { annot :: ea,
        num :: Int
      }
  | IdentifierExpr
      { annot :: ea,
        id :: Ident
      }
  | Call
      { annot :: ea,
        id :: Ident,
        args :: [Expr ea]
      }
  deriving (Show, Eq)

data Ty
  = I32
  | Bool
  | Void
  | FunTy
      { argtys :: [Ty],
        retty :: Ty
      }
  deriving (Show, Eq)

data VarDef = VarDef
  { id :: Ident,
    ty :: Ty
  }
  deriving (Show, Eq)

data Stmt ea ba
  = ExprStmt {expr :: Expr ea}
  | LetStmt
      { vardef :: VarDef,
        expr :: Expr ea
      }
  | AssignStmt
      { id :: Ident,
        expr :: Expr ea
      }
  | ReturnStmt
      { expr :: Expr ea
      }
  | IfStmt
      { cond :: Expr ea,
        ifBody :: Block ea ba,
        elseBody :: Maybe (Block ea ba)
      }
  | WhileStmt
      { cond :: Expr ea,
        body :: Block ea ba
      }
  deriving (Show, Eq)

data Block ea ba = Block
  { annot :: ba,
    stmts :: [Stmt ea ba]
  }
  deriving (Show, Eq)

data Fun a ba = Fun
  { id :: Ident,
    args :: [VarDef],
    retty :: Ty,
    body :: Block a ba
  }
  deriving (Show, Eq)

newtype Program a ba = Program
  { funcs :: [Fun a ba]
  }
  deriving (Show, Eq)

type RawExpr = Expr ()

type RawStmt = Stmt () ()

type RawBlock = Block () ()

type RawFun = Fun () ()

type RawProgram = Program () ()

exprAnnot :: Expr ea -> ea
exprAnnot (BinExpr {annot}) = annot
exprAnnot (NumberExpr {annot}) = annot
exprAnnot (IdentifierExpr {annot}) = annot
exprAnnot (Call {annot}) = annot
