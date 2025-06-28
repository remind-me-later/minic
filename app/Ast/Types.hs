{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ast.Types where

type Id = String

data Ty
  = IntTy
  | BoolTy
  | VoidTy
  | FunTy
      { args :: [Ty],
        retty :: Ty
      }
  | ArrTy
      { elemTy :: Ty,
        size :: Int
      }
  deriving (Show, Eq)

tySize :: Ty -> Int
tySize IntTy = 8
tySize BoolTy = 8
tySize VoidTy = 0
tySize (FunTy {args}) = sum (map tySize args)
tySize (ArrTy {elemTy, size}) = tySize elemTy * size

data BinOp
  = -- Arithmetic
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | -- Logical
    And
  | Or
  | Xor
  | -- Comparison
    Equal
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | NotEqual
  deriving (Eq)

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Equal = "=="
  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanOrEqual = "<="
  show GreaterThanOrEqual = ">="
  show NotEqual = "!="
  show And = "&&"
  show Or = "||"
  show Xor = "^"
  show Mod = "%"

data UnaryOp
  = UnarySub
  | UnaryNot
  deriving (Eq)

instance Show UnaryOp where
  show UnarySub = "-"
  show UnaryNot = "!"

data Exp ea = Exp
  { annot :: ea,
    exp :: ExpInner ea
  }
  deriving (Show, Eq)

data ExpInner ea
  = BinExp
      { left :: Exp ea,
        op :: BinOp,
        right :: Exp ea
      }
  | UnaryExp
      { unop :: UnaryOp,
        exp :: Exp ea
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
  | ArrAccess
      { id :: Id,
        index :: Exp ea
      }
  deriving (Eq)

instance (Show ea) => Show (ExpInner ea) where
  show (BinExp left op right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
  show (UnaryExp unop exp) = show unop ++ show exp
  show (NumberExp num) = show num
  show (IdExp id) = id
  show (Call id args) = id ++ "(" ++ unwords (map show args)
  show (ArrAccess id index) = id ++ "[" ++ show index ++ "]"

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
  | LetArrStmt
      { vardef :: VarDef,
        size :: Int,
        elems :: [Exp ea]
      }
  | AssignStmt
      { id :: Id,
        exp :: Exp ea
      }
  | AssignArrStmt
      { id :: Id,
        index :: Exp ea,
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

data ExternFun = ExternFun
  { id :: Id,
    args :: [Ty],
    retty :: Ty
  }
  deriving (Show, Eq)

data Program a ba = Program
  { annot :: ba,
    funcs :: [Fun a ba],
    externFuns :: [ExternFun],
    mainFun :: Maybe (Fun a ba)
  }
  deriving (Show, Eq)

type RawExp = Exp ()

type RawStmt = Stmt () ()

type RawBlock = Block () ()

type RawFun = Fun () ()

type RawExternFun = ExternFun

type RawProgram = Program () ()
