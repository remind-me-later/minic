{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ast
  ( Exp (..),
    Stmt (..),
    Block (..),
    Fun (..),
    Program (..),
    Operator (..),
    Ty (..),
    Ident,
    VarDef (..),
    RawExp,
    RawStmt,
    RawBlock,
    RawFun,
    RawProgram,
    ExpInner (..),
  )
where

type Ident = String

data Ty
  = Int
  | Bool
  | Void
  | FunTy
      { argtys :: [Ty],
        retty :: Ty
      }
  deriving (Show, Eq)

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
  deriving (Eq)

instance Show Operator where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Assign = "="
  show Equal = "=="
  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanOrEqual = "<="
  show GreaterThanOrEqual = ">="
  show NotEqual = "!="
  show And = "&&"
  show Or = "||"
  show Not = "!"
  show Xor = "^"
  show Modulo = "%"

data Exp ea = Exp
  { annot :: ea,
    exp :: ExpInner ea
  }
  deriving (Show, Eq)

data ExpInner ea
  = BinExp
      { left :: Exp ea,
        op :: Operator,
        right :: Exp ea
      }
  | NumberExp
      { num :: Int
      }
  | IdentifierExp
      { id :: Ident
      }
  | Call
      { id :: Ident,
        args :: [Exp ea]
      }
  deriving (Show, Eq)

data VarDef = VarDef
  { id :: Ident,
    ty :: Ty
  }
  deriving (Show, Eq)

data Stmt ea ba
  = ExpStmt {exp :: Exp ea}
  | LetStmt
      { vardef :: VarDef,
        exp :: Exp ea
      }
  | AssignStmt
      { id :: Ident,
        exp :: Exp ea
      }
  | ReturnStmt
      { exp :: Exp ea
      }
  | IfStmt
      { cond :: Exp ea,
        ifBody :: Block ea ba,
        elseBody :: Maybe (Block ea ba)
      }
  | WhileStmt
      { cond :: Exp ea,
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

type RawExp = Exp ()

type RawStmt = Stmt () ()

type RawBlock = Block () ()

type RawFun = Fun () ()

type RawProgram = Program () ()
