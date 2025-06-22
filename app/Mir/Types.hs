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
    ExternFun (..),
  )
where

import Ast qualified

type Temp = Int

type Label = String

data Var
  = Local {id :: Ast.Id}
  | Arg {id :: Ast.Id}
  deriving (Eq)

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
  = Assign {dst :: Temp, srcOp :: Operand}
  | UnaryOp {dst :: Temp, unop :: Ast.UnaryOp, src :: Temp}
  | BinOp {dst :: Temp, binop :: Ast.BinOp, left :: Temp, right :: Temp}
  | Load {dst :: Temp, srcVar :: Var}
  | Store {dstVar :: Var, src :: Temp}
  | Call {ret :: Maybe Temp, funId :: Ast.Id, argCount :: Int}
  | Param {param :: Temp}
  | Return {retVal :: Maybe Temp}
  | Jump {target :: Label}
  | CondJump {cond :: Temp, trueLabel :: Label, falseLabel :: Label}
  deriving (Eq)

instance Show Inst where
  show Assign {dst, srcOp} = "t" ++ show dst ++ " = " ++ show srcOp
  show UnaryOp {dst, unop, src} = "t" ++ show dst ++ " = " ++ show unop ++ " t" ++ show src
  show BinOp {dst, binop, left, right} =
    "t" ++ show dst ++ " = " ++ "t" ++ show left ++ " " ++ show binop ++ " " ++ "t" ++ show right
  show Load {dst, srcVar} = "t" ++ show dst ++ " = " ++ show srcVar
  show Store {dstVar, src} = show dstVar ++ " = t" ++ show src
  show Call {ret = Just t, funId, argCount} = "t" ++ show t ++ " = call " ++ funId ++ ", " ++ show argCount
  show Call {ret = Nothing, funId, argCount} = "call " ++ funId ++ ", " ++ show argCount
  show Param {param} = "param " ++ "t" ++ show param
  show Return {retVal = Just t} = "return " ++ "t" ++ show t
  show Return {retVal = Nothing} = "return"
  show Jump {target} = "goto " ++ target
  show CondJump {cond, trueLabel, falseLabel} =
    "if " ++ "t" ++ show cond ++ " then goto " ++ trueLabel ++ " else goto " ++ falseLabel

-- A Basic Block is a sequence of instructions that starts with a label
-- and ends with a control flow instruction (Jump, CondJump, Return).
-- For simplicity here, we'll just list instructions and assume the last one is control flow.
-- A more rigorous CFG would explicitly link blocks.
data BasicBlock = BasicBlock
  { label :: Label,
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
