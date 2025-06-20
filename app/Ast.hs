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
    Id,
    VarDef (..),
    RawExp,
    RawStmt,
    RawBlock,
    RawFun,
    RawProgram,
    ExpInner (..),
  )
where

type Id = String

data Ty
  = IntTy
  | BoolTy
  | VoidTy
  | FunTy
      { argtys :: [Ty],
        retty :: Ty
      }
  deriving (Show, Eq)

data Operator
  = Add
  | Sub
  | Mul
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
  show Sub = "-"
  show Mul = "*"
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
  | IdExp
      { id :: Id
      }
  | Call
      { id :: Id,
        args :: [Exp ea]
      }
  deriving (Show, Eq)

data VarDef = VarDef
  { id :: Id,
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
      { id :: Id,
        exp :: Exp ea
      }
  | ReturnStmt
      { retexp :: Maybe (Exp ea)
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
  { id :: Id,
    args :: [VarDef],
    retty :: Ty,
    body :: Block a ba
  }
  deriving (Show, Eq)

data Program a ba = Program
  { annot :: ba,
    funcs :: [Fun a ba]
  }
  deriving (Show, Eq)

type RawExp = Exp ()

type RawStmt = Stmt () ()

type RawBlock = Block () ()

type RawFun = Fun () ()

type RawProgram = Program () ()
