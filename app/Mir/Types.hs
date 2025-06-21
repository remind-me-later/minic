{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Types
  ( Temp,
    Label,
    Var (..),
    Operand (..),
    Inst (..),
    BasicBlock (..),
    Fun (..),
    Program (..),
    varId,
    ExternFun (..),
  )
where

import Ast qualified
  ( Id,
    Operator,
  )

type Temp = Int

type Label = String

data Var
  = Local Ast.Id
  | Arg Ast.Id
  deriving (Eq)

varId :: Var -> Ast.Id
varId (Local id) = id
varId (Arg id) = id

instance Show Var where
  show (Local id) = "local " ++ id
  show (Arg id) = "arg " ++ id

data Operand
  = ConstInt Int
  | Temp Temp
  deriving (Eq)

instance Show Operand where
  show (ConstInt n) = "const " ++ show n
  show (Temp t) = "t" ++ show t

data Inst
  = Assign Temp Operand
  | BinOp Temp Ast.Operator Temp Temp
  | Load Temp Var
  | Store Var Temp
  | Call (Maybe Temp) Ast.Id Int
  | Param Temp
  | Return (Maybe Temp)
  | Jump Label
  | CondJump Temp Label Label
  deriving (Eq)

instance Show Inst where
  show (Assign t op) = "t" ++ show t ++ " = " ++ show op
  show (BinOp t1 op t2 t3) = "t" ++ show t1 ++ " = " ++ "t" ++ show t2 ++ " " ++ show op ++ " " ++ "t" ++ show t3
  show (Load t v) = "t" ++ show t ++ " = " ++ show v
  show (Store v t) = show v ++ " = t" ++ show t
  show (Call (Just t) id n) = "t" ++ show t ++ " = call " ++ id ++ ", " ++ show n
  show (Call Nothing id n) = "call " ++ id ++ ", " ++ show n
  show (Param t) = "param " ++ "t" ++ show t
  show (Return (Just t)) = "return " ++ "t" ++ show t
  show (Return Nothing) = "return"
  show (Jump label) = "goto " ++ label
  show (CondJump t trueLabel falseLabel) =
    "if " ++ "t" ++ show t ++ " then goto " ++ trueLabel ++ " else goto " ++ falseLabel

-- A Basic Block is a sequence of instructions that starts with a label
-- and ends with a control flow instruction (Jump, CondJump, Return).
-- For simplicity here, we'll just list instructions and assume the last one is control flow.
-- A more rigorous CFG would explicitly link blocks.
data BasicBlock = BasicBlock
  { blockLabel :: Label,
    insts :: [Inst]
  }
  deriving (Eq)

instance Show BasicBlock where
  show (BasicBlock label instructions) =
    label ++ ":\n" ++ unlines (("  " ++) . show <$> instructions)

data Fun = Fun
  { id :: Ast.Id,
    args :: [Ast.Id],
    locals :: [Ast.Id],
    entryLabel :: Label,
    blocks :: [BasicBlock]
  }
  deriving (Eq)

instance Show Fun where
  show (Fun name args locals entryLabel blocks) =
    "Function: "
      ++ name
      ++ "\n"
      ++ "Args: "
      ++ unwords args
      ++ "\n"
      ++ "Locals: "
      ++ unwords locals
      ++ "\n"
      ++ "Entry: "
      ++ entryLabel
      ++ "\n"
      ++ "Blocks:\n"
      ++ unlines (reverse $ show <$> blocks)

newtype ExternFun = ExternFun
  { externId :: Ast.Id
  }
  deriving (Eq)

instance Show ExternFun where
  show (ExternFun id) = "Extern Function: " ++ id

data Program = Program
  { funs :: [Fun],
    externFuns :: [ExternFun],
    mainFun :: Maybe Fun
  }
  deriving (Eq)

instance Show Program where
  show (Program functions externFuns mainFun) =
    "Program:\n"
      ++ "Extern Functions:\n"
      ++ unlines (show <$> externFuns)
      ++ "Functions:\n"
      ++ unlines (show <$> functions)
      ++ case mainFun of
        Just main -> "Main Function:\n" ++ show main
        Nothing -> "No Main Function"
