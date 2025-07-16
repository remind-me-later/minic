module Ast.Types
  ( Exp (..),
    ExpInner (..),
    VarDef (..),
    Stmt (..),
    Block (..),
    Fun (..),
    ExternFun (..),
    Program (..),
    RawExp,
    RawStmt,
    RawBlock,
    RawFun,
    RawExternFun,
    RawProgram,
  )
where

import TypeSystem

data Exp ea = Exp
  { _expAnnot :: ea,
    _expInner :: ExpInner ea
  }
  deriving (Show, Eq)

data ExpInner ea
  = BinExp
      { binLeft :: Exp ea,
        binOp :: BinOp,
        binRight :: Exp ea
      }
  | UnaryExp
      { unaryOp :: UnaryOp,
        unaryExp :: Exp ea
      }
  | NumberExp
      { numberValue :: Int
      }
  | CharExp
      { charValue :: Char
      }
  | IdExp
      { idName :: Id
      }
  | Call
      { callId :: Id,
        callArgs :: [Exp ea]
      }
  | ArrAccess
      { arrId :: Id,
        arrIndex :: Exp ea
      }
  | TakeAddress
      { takeAddressId :: Id
      }
  deriving (Eq)

instance (Show ea) => Show (ExpInner ea) where
  show (BinExp left op right) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
  show (UnaryExp unop unexp) = show unop ++ show unexp
  show (NumberExp num) = show num
  show (CharExp char) = "'" ++ [char] ++ "'"
  show (IdExp identifier) = identifier
  show (Call identifier args) = identifier ++ "(" ++ unwords (map show args)
  show (ArrAccess identifier index) = identifier ++ "[" ++ show index ++ "]"
  show (TakeAddress identifier) = "&" ++ identifier

data VarDef = VarDef
  { _varDefId :: Id,
    _varDefTy :: Ty
  }
  deriving (Show, Eq)

data Stmt ea
  = ExpStmt {stmtExp :: Exp ea}
  | LetStmt
      { letVarDef :: VarDef,
        letExp :: Exp ea,
        letStorage :: Maybe StorageSpecifier
      }
  | LetArrStmt
      { letArrVarDef :: VarDef,
        letArrSize :: Int,
        letArrElems :: [Exp ea],
        letArrStorage :: Maybe StorageSpecifier
      }
  | AssignStmt
      { assignId :: Id,
        assignExp :: Exp ea
      }
  | AssignArrStmt
      { assignArrId :: Id,
        assignArrIndex :: Exp ea,
        assignArrExp :: Exp ea
      }
  | ReturnStmt
      { returnExp :: Maybe (Exp ea)
      }
  | IfStmt
      { ifCond :: Exp ea,
        ifBody :: Block ea,
        ifElseBody :: Maybe (Block ea)
      }
  | WhileStmt
      { whileCond :: Exp ea,
        whileBody :: Block ea
      }
  | ForStmt
      { forInit :: Stmt ea,
        forCond :: Exp ea,
        forUpdate :: Stmt ea,
        forBody :: Block ea
      }
  deriving (Show, Eq)

data Block ea = Block
  { _blockId :: Int,
    _blockStmts :: [Stmt ea]
  }
  deriving (Show, Eq)

data Fun a = Fun
  { _funId :: Id,
    _funArgs :: [VarDef],
    _funRetTy :: Ty,
    _funBody :: Block a
  }
  deriving (Show, Eq)

data ExternFun = ExternFun
  { externFunId :: Id,
    externFunArgs :: [Ty],
    externFunRetTy :: Ty
  }
  deriving (Show, Eq)

data Program a = Program
  { programFuncs :: [Fun a],
    programExternFuns :: [ExternFun],
    programMainFun :: Maybe (Fun a)
  }
  deriving (Show, Eq)

type RawExp = Exp ()

type RawStmt = Stmt ()

type RawBlock = Block ()

type RawFun = Fun ()

type RawExternFun = ExternFun

type RawProgram = Program ()
