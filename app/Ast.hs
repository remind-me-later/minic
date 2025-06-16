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
    Annotated (..),
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

data Expr a
  = BinExpr
      { annot :: a,
        leftExpr :: Expr a,
        binOp :: Operator,
        rightExpr :: Expr a
      }
  | NumberExpr
      { annot :: a,
        numLiteral :: Int
      }
  | IdentifierExpr
      { annot :: a,
        ident :: Ident
      }
  | Call
      { annot :: a,
        funCallName :: Ident,
        funCallArgs :: [Expr a]
      }
  deriving (Show, Eq)

data Ty
  = I32
  | Bool
  | Void
  | FunTy [Ty] Ty
  deriving (Show, Eq)

data VarDef = VarDef Ident Ty deriving (Show, Eq)

data Stmt a
  = ExprStmt (Expr a)
  | LetStmt
      { annot :: a,
        vardef :: VarDef,
        expr :: Expr a
      }
  | AssignStmt
      { annot :: a,
        id :: Ident,
        expr :: Expr a
      }
  | ReturnStmt
      { annot :: a,
        expr :: Expr a
      }
  | IfStmt
      { annot :: a,
        cond :: Expr a,
        ifBody :: Block a,
        elseBody :: Maybe (Block a)
      }
  | WhileStmt
      { annot :: a,
        cond :: Expr a,
        body :: Block a
      }
  deriving (Show, Eq)

data Block a = Block
  { annot :: a,
    stmts :: [Stmt a]
  }
  deriving (Show, Eq)

data Fun a = Fun
  { annot :: a,
    id :: Ident,
    args :: [VarDef],
    retty :: Ty,
    body :: Block a
  }
  deriving (Show, Eq)

newtype Program a = Program [Fun a] deriving (Show, Eq)

type RawExpr = Expr ()

type RawStmt = Stmt ()

type RawBlock = Block ()

type RawFun = Fun ()

type RawProgram = Program ()

instance Functor Expr where
  fmap f (BinExpr annot left op right) =
    BinExpr {annot = f annot, leftExpr = fmap f left, binOp = op, rightExpr = fmap f right}
  fmap f (NumberExpr annot num) = NumberExpr {annot = f annot, numLiteral = num}
  fmap f (IdentifierExpr annot ident) = IdentifierExpr (f annot) ident
  fmap f (Call annot name args) = Call (f annot) name (map (fmap f) args)

instance Functor Stmt where
  fmap f (ExprStmt expr) = ExprStmt (fmap f expr)
  fmap f (LetStmt annot varDef expr) =
    LetStmt {annot = f annot, vardef = varDef, expr = fmap f expr}
  fmap f (AssignStmt annot varName expr) =
    AssignStmt {annot = f annot, id = varName, expr = fmap f expr}
  fmap f (ReturnStmt annot expr) = ReturnStmt {annot = f annot, expr = fmap f expr}
  fmap f (IfStmt annot cond body elseBody) =
    IfStmt {annot = f annot, cond = fmap f cond, ifBody = fmap f body, elseBody = fmap (fmap f) elseBody}
  fmap f (WhileStmt annot cond body) =
    WhileStmt {annot = f annot, cond = fmap f cond, body = fmap f body}

instance Functor Block where
  fmap f (Block annot stmts) =
    Block {annot = f annot, stmts = map (fmap f) stmts}

instance Functor Fun where
  fmap f (Fun annot id args retty body) =
    Fun {annot = f annot, id = id, args = args, retty = retty, body = fmap f body}

instance Functor Program where
  fmap f (Program funcs) = Program (map (fmap f) funcs)

class Annotated t a where
  annotation :: t a -> a
  setAnnotation :: t a -> a -> t a

instance Annotated Expr a where
  annotation (BinExpr annot _ _ _) = annot
  annotation (NumberExpr annot _) = annot
  annotation (IdentifierExpr annot _) = annot
  annotation (Call annot _ _) = annot

  setAnnotation (BinExpr _ left op right) newAnnot =
    BinExpr {annot = newAnnot, leftExpr = left, binOp = op, rightExpr = right}
  setAnnotation (NumberExpr _ num) newAnnot =
    NumberExpr {annot = newAnnot, numLiteral = num}
  setAnnotation (IdentifierExpr _ ident) newAnnot =
    IdentifierExpr {annot = newAnnot, ident = ident}
  setAnnotation (Call _ name args) newAnnot =
    Call {annot = newAnnot, funCallName = name, funCallArgs = args}

instance Annotated Stmt a where
  annotation (ExprStmt expr) = annotation expr
  annotation (LetStmt annot _ _) = annot
  annotation (AssignStmt annot _ _) = annot
  annotation (ReturnStmt annot _) = annot
  annotation (IfStmt annot _ _ _) = annot
  annotation (WhileStmt annot _ _) = annot

  setAnnotation (ExprStmt expr) newAnnot =
    ExprStmt (setAnnotation expr newAnnot)
  setAnnotation (LetStmt _ varDef expr) newAnnot =
    LetStmt {annot = newAnnot, vardef = varDef, expr = setAnnotation expr newAnnot}
  setAnnotation (AssignStmt _ varName expr) newAnnot =
    AssignStmt {annot = newAnnot, id = varName, expr = setAnnotation expr newAnnot}
  setAnnotation (ReturnStmt _ expr) newAnnot =
    ReturnStmt {annot = newAnnot, expr = setAnnotation expr newAnnot}
  setAnnotation (IfStmt _ cond body elseBody) newAnnot =
    IfStmt {annot = newAnnot, cond = setAnnotation cond newAnnot, ifBody = body, elseBody = elseBody}
  setAnnotation (WhileStmt _ cond body) newAnnot =
    WhileStmt {annot = newAnnot, cond = setAnnotation cond newAnnot, body = body}

instance Annotated Block a where
  annotation (Block annot _) = annot

  setAnnotation (Block _ stmts) newAnnot =
    Block {annot = newAnnot, stmts}

instance Annotated Fun a where
  annotation (Fun annot _ _ _ _) = annot

  setAnnotation (Fun _ id args retty body) newAnnot =
    Fun {annot = newAnnot, id = id, args = args, retty = retty, body}
