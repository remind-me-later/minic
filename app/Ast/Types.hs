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
  { expAnnot :: ea,
    expInner :: ExpInner ea
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
  { varDefId :: Id,
    varDefTy :: Ty
  }
  deriving (Show, Eq)

data Stmt ea ba
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
        ifBody :: Block ea ba,
        ifElseBody :: Maybe (Block ea ba)
      }
  | WhileStmt
      { whileCond :: Exp ea,
        whileBody :: Block ea ba
      }
  | ForStmt
      { forInit :: Stmt ea ba,
        forCond :: Exp ea,
        forUpdate :: Stmt ea ba,
        forBody :: Block ea ba
      }
  deriving (Show, Eq)

data Block ea ba = Block
  { blockAnnot :: ba,
    blockStmts :: [Stmt ea ba]
  }
  deriving (Show, Eq)

data Fun a ba = Fun
  { funId :: Id,
    funArgs :: [VarDef],
    funRetTy :: Ty,
    funBody :: Block a ba
  }
  deriving (Show, Eq)

data ExternFun = ExternFun
  { externFunId :: Id,
    externFunArgs :: [Ty],
    externFunRetTy :: Ty
  }
  deriving (Show, Eq)

data Program a ba = Program
  { programAnnot :: ba,
    programFuncs :: [Fun a ba],
    programExternFuns :: [ExternFun],
    programMainFun :: Maybe (Fun a ba)
  }
  deriving (Show, Eq)

type RawExp = Exp ()

type RawStmt = Stmt () ()

type RawBlock = Block () ()

type RawFun = Fun () ()

type RawExternFun = ExternFun

type RawProgram = Program () ()
