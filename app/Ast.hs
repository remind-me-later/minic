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
    getExprAnnotations,
    getStmtAnnotations,
    getBlockAnnotations,
    getFunAnnotations,
    getProgramAnnotations,
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
      { binAnnot :: a,
        leftExpr :: Expr a,
        binOp :: Operator,
        rightExpr :: Expr a
      }
  | NumberExpr
      { numAnnot :: a,
        numLiteral :: Int
      }
  | IdentifierExpr
      { identAnnot :: a,
        ident :: Ident
      }
  | Call
      { callAnnot :: a,
        funCallName :: Ident,
        funCallArgs :: [Expr a]
      }
  deriving (Show, Eq)

data Ty
  = I32
  | Bool
  | Void
  deriving (Show, Eq)

data VarDef = VarDef Ident Ty deriving (Show, Eq)

data Stmt a
  = ExprStmt (Expr a)
  | LetStmt
      { letVarDef :: VarDef,
        letVarExpr :: Expr a
      }
  | AssignStmt
      { assignVarName :: Ident,
        assignExpr :: Expr a
      }
  | ReturnStmt
      { returnExpr :: Expr a
      }
  | IfStmt
      { ifCond :: Expr a,
        ifBody :: Block a,
        elseBody :: Maybe (Block a)
      }
  | WhileStmt
      { whileCond :: Expr a,
        whileBody :: Block a
      }
  deriving (Show, Eq)

newtype Block a = Block [Stmt a] deriving (Show, Eq)

data Fun a = Fun
  { funName :: Ident,
    funArgs :: [VarDef],
    funRetTy :: Ty,
    funBody :: Block a
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
    BinExpr (f annot) (fmap f left) op (fmap f right)
  fmap f (NumberExpr annot num) = NumberExpr (f annot) num
  fmap f (IdentifierExpr annot ident) = IdentifierExpr (f annot) ident
  fmap f (Call annot name args) = Call (f annot) name (map (fmap f) args)

instance Functor Stmt where
  fmap f (ExprStmt expr) = ExprStmt (fmap f expr)
  fmap f (LetStmt varDef expr) = LetStmt varDef (fmap f expr)
  fmap f (AssignStmt name expr) = AssignStmt name (fmap f expr)
  fmap f (ReturnStmt expr) = ReturnStmt (fmap f expr)
  fmap f (IfStmt cond body elseBody) =
    IfStmt (fmap f cond) (fmap f body) (fmap (fmap f) elseBody)
  fmap f (WhileStmt cond body) = WhileStmt (fmap f cond) (fmap f body)

instance Functor Block where
  fmap f (Block stmts) = Block (map (fmap f) stmts)

instance Functor Fun where
  fmap f (Fun name args retTy body) =
    Fun name args retTy (fmap f body)

instance Functor Program where
  fmap f (Program funcs) = Program (map (fmap f) funcs)

getExprAnnotations :: Expr a -> a
getExprAnnotations (BinExpr annot _ _ _) = annot
getExprAnnotations (NumberExpr annot _) = annot
getExprAnnotations (IdentifierExpr annot _) = annot
getExprAnnotations (Call annot _ _) = annot

getStmtAnnotations :: Stmt a -> a
getStmtAnnotations (ExprStmt expr) = getExprAnnotations expr
getStmtAnnotations (LetStmt _ expr) = getExprAnnotations expr
getStmtAnnotations (AssignStmt _ expr) = getExprAnnotations expr
getStmtAnnotations (ReturnStmt expr) = getExprAnnotations expr
getStmtAnnotations (IfStmt cond _ _) = getExprAnnotations cond
getStmtAnnotations (WhileStmt cond _) = getExprAnnotations cond

getBlockAnnotations :: Block a -> a
getBlockAnnotations (Block stmts) =
  if null stmts
    then error "Block has no statements to get annotations from"
    else getStmtAnnotations (head stmts)

getFunAnnotations :: Fun a -> a
getFunAnnotations (Fun _ _ _ body) = getBlockAnnotations body

getProgramAnnotations :: Program a -> a
getProgramAnnotations (Program funcs) =
  if null funcs
    then error "Program has no functions to get annotations from"
    else getFunAnnotations (head funcs)